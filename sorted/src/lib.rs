use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, visit_mut::VisitMut, Item, Pat, Path};

struct OrderChecker {
    seen: Vec<String>,
}
impl OrderChecker {
    fn new() -> Self {
        Self { seen: Vec::new() }
    }
    fn try_add(&mut self, new_elem: String) -> Result<(), String> {
        let first_less_than = self.seen.iter().find(|&n| &new_elem < n);
        if let Some(l) = first_less_than {
            return Err(format!("{} should sort before {}", new_elem, l));
        }
        self.seen.push(new_elem);
        Ok(())
    }
}
struct MatchChecker {
    checker_stack: Vec<Option<OrderChecker>>,
    errs: Vec<syn::Error>,
}
impl MatchChecker {
    fn new() -> Self {
        Self {
            errs: Vec::new(),
            checker_stack: Vec::new(),
        }
    }
}

fn path_to_string_and_span(p: &Path) -> (String, proc_macro2::TokenStream) {
    (
        p.segments
            .iter()
            .map(|x| x.ident.to_string())
            .collect::<Vec<_>>()
            .join("::"),
        p.into_token_stream(),
    )
}

impl syn::visit_mut::VisitMut for MatchChecker {
    fn visit_pat_mut(&mut self, i: &mut Pat) {
        if let Some(Some(checker)) = self.checker_stack.last_mut() {
            let id = match i {
                Pat::Path(p) => Some(path_to_string_and_span(&p.path)),
                Pat::TupleStruct(p) => Some(path_to_string_and_span(&p.path)),
                Pat::Struct(p) => Some(path_to_string_and_span(&p.path)),
                Pat::Ident(s) => Some((s.ident.to_string(), s.into_token_stream())),
                Pat::Wild(x) => Some(("_".to_owned(), x.to_token_stream())),
                _ => None,
            };
            match id {
                Some((id_str, span)) => {
                    if let Err(x) = checker.try_add(id_str) {
                        self.errs.push(syn::Error::new_spanned(span, x));
                    }
                }
                None => self
                    .errs
                    .push(syn::Error::new_spanned(i, "unsupported by #[sorted]")),
            }
        }
    }

    fn visit_expr_match_mut(&mut self, i: &mut syn::ExprMatch) {
        let mut taken = Vec::new();
        std::mem::swap(&mut i.attrs, &mut taken);
        let (sorted_attrs, rest) = taken.into_iter().partition(|a| a.path.is_ident("sorted"));
        i.attrs = rest;
        if !sorted_attrs.is_empty() {
            self.checker_stack.push(Some(OrderChecker::new()));
        } else {
            self.checker_stack.push(None)
        }
        syn::visit_mut::visit_expr_match_mut(self, i);
        self.checker_stack.pop();
    }
}

#[proc_macro_attribute]
pub fn sorted(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut input2 = input.clone();
    let parsed_input = parse_macro_input!(input as Item);
    if let Err(x) = go(parsed_input) {
        input2.extend::<proc_macro::TokenStream>(x.to_compile_error().into());
    }
    input2
}
#[proc_macro_attribute]
pub fn check(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut parsed_input = parse_macro_input!(input as Item);
    let mut visiter = MatchChecker::new();
    visiter.visit_item_mut(&mut parsed_input);
    let mut result = quote! {
        #parsed_input
    };
    if let Some(e) = visiter.errs.into_iter().next() {
        result.extend(e.into_compile_error())
    }
    result.into()
}

fn go(parsed_input: Item) -> syn::Result<()> {
    if let Item::Enum(e) = parsed_input {
        let mut oc = OrderChecker::new();
        for v in e.variants {
            if let Err(l) = oc.try_add(v.ident.to_string()) {
                return Err(syn::Error::new(v.span(), l));
            }
        }
        Ok(())
    } else {
        Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected enum or match expression",
        ))
    }
}
