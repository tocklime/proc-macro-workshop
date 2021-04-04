use std::{collections::HashSet, convert::TryFrom};

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Data, DeriveInput, GenericParam, Generics,
    Meta, Path, Type,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    mk_derive(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
#[derive(Debug)]
struct FieldDef<'a> {
    name: &'a Ident,
    ty: &'a Type,
    fmt: Option<String>,
}
impl<'a> TryFrom<&'a syn::Field> for FieldDef<'a> {
    type Error = syn::Error;

    fn try_from(f: &'a syn::Field) -> Result<Self, Self::Error> {
        let name = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        let mut fmt = None;
        let builder_attrs = f
            .attrs
            .iter()
            .filter(|a| a.path.is_ident("debug"))
            .collect::<Vec<_>>();
        for at in builder_attrs {
            let meta = at.parse_meta()?;
            if let Meta::NameValue(mnv) = meta {
                if let syn::Lit::Str(s) = &mnv.lit {
                    fmt = Some(s.value())
                }
            }
        }
        Ok(Self { name, ty, fmt })
    }
}
fn fields(data: &Data, span_for_err: Span) -> Result<Vec<FieldDef>, syn::Error> {
    if let Data::Struct(s) = data {
        if let syn::Fields::Named(fields) = &s.fields {
            return fields.named.iter().map(|f| FieldDef::try_from(f)).collect();
        }
    }
    Err(syn::Error::new(
        span_for_err,
        "Expected struct with named fields",
    ))
}
#[derive(Default)]
struct GenericAnalysis<'a> {
    declared_generics: HashSet<&'a Ident>,
    used_generics: HashSet<&'a Ident>,
    used_assocs: HashSet<&'a Path>,
}

impl<'a> GenericAnalysis<'a> {
    fn set_declared(&mut self, generics: &'a Generics) -> &mut Self {
        self.declared_generics = generics.type_params().map(|x| &x.ident).collect();
        self
    }
    fn analyse_use(&mut self, t: &'a Type) {
        if let Type::Path(p) = t {
            let ident_0 = &p.path.segments[0].ident;
            if self.declared_generics.contains(ident_0) {
                if p.path.segments.len() == 1 {
                    self.used_generics.insert(ident_0);
                } else {
                    self.used_assocs.insert(&p.path);
                }
            } else if ident_0 == "PhantomData" {
                //skip.
            } else {
                for s in &p.path.segments {
                    if let syn::PathArguments::AngleBracketed(ab) = &s.arguments {
                        for a in &ab.args {
                            if let syn::GenericArgument::Type(t) = a {
                                self.analyse_use(t)
                            }
                        }
                    }
                }
            }
        }
    }
}

fn mk_debug_fields(fields: &[FieldDef]) -> proc_macro2::TokenStream {
    let list = fields.iter().map(|FieldDef { name, fmt, .. }| {
        let name_as_str = proc_macro2::Literal::string(&name.to_string());

        match fmt {
            Some(f) => {
                let f_as_str = proc_macro2::Literal::string(f);
                quote! { .field(#name_as_str, &format_args!(#f_as_str, &self.#name))}
            }
            None => quote! { .field(#name_as_str, &self.#name) },
        }
    });
    quote! { #(#list)* }
}
fn add_trait_bounds(mut generics: Generics, used_generics: &HashSet<&Ident>) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            if used_generics.contains(&type_param.ident) {
                type_param.bounds.push(parse_quote!(std::fmt::Debug))
            }
        }
    }
    generics
}

fn mk_derive(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = &input.ident;
    let name_as_str = proc_macro2::Literal::string(&name.to_string());
    let fs = fields(&input.data, input.span())?;
    let mut gens = GenericAnalysis::default();
    let my_generics = input.generics.clone();
    gens.set_declared(&my_generics);
    for fd in &fs {
        gens.analyse_use(fd.ty);
    }
    let field_uses = mk_debug_fields(&fs);
    let generics = add_trait_bounds(input.generics, &gens.used_generics);
    let (impl_generics, ty_generics, _where_clause) = generics.split_for_impl();

    let where_clause = if gens.used_assocs.is_empty() {
        proc_macro2::TokenStream::new()
    } else {
        let assocs = gens.used_assocs.iter().map(|t| {
            quote! { #t : std::fmt::Debug }
        });
        quote! { where #(#assocs),* }
    };

    Ok(quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                f.debug_struct(#name_as_str)
                 #field_uses
                 .finish()
            }
        }
    })
}
