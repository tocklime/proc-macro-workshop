use proc_macro::TokenStream;
use syn::{Item, Pat, parse_macro_input, spanned::Spanned, visit_mut::VisitMut};
use quote::quote;

struct OrderChecker {
    seen: Vec<String>
}
impl OrderChecker {
    fn new() -> Self {
        Self {seen: Vec::new()}
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
struct MatchChecker{
    checker_stack : Vec<Option<OrderChecker>>,
    errs : Vec<syn::Error>
}
impl MatchChecker {
    fn new() -> Self {
        Self {errs: Vec::new(), checker_stack: Vec::new()}
    }
}

impl syn::visit_mut::VisitMut for MatchChecker {
    fn visit_pat_mut(&mut self, i: &mut Pat) {
        let id = match i {
            Pat::Path(p) => {Some(&p.path)}
            Pat::TupleStruct(ts) => {Some(&ts.path)}
            Pat::Struct(s) => {Some(&s.path)}
            _ => None
        };
        if let Some(path) = id {
            let id_str = path.segments.iter().map(|x| x.ident.to_string()).collect::<Vec<_>>().join("::");
            if let Some(checker) = self.checker_stack.last_mut().unwrap() {
                if let Err(x) = checker.try_add(id_str) {
                    self.errs.push(syn::Error::new_spanned(path, x));
                }
            }
        }
    }

    fn visit_expr_match_mut(&mut self, i: &mut syn::ExprMatch) {
        let mut taken = Vec::new();
        std::mem::swap(&mut i.attrs, &mut taken);
        let (sorted_attrs,rest) = taken.into_iter().partition(|a| a.path.is_ident("sorted"));
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
    for e in visiter.errs {
        result.extend(e.into_compile_error())
    }
    result.into()
}

fn go(parsed_input: Item) -> syn::Result<()> {
    if let Item::Enum(e) = parsed_input {
        let mut oc = OrderChecker::new();
        for v in e.variants {
            if let Err(l) = oc.try_add(v.ident.to_string()) {
                return Err(syn::Error::new( v.span(),l));
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
