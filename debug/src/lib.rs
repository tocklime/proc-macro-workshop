use std::{collections::HashSet, convert::TryFrom};

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Attribute, Data, DeriveInput, GenericParam,
    Generics, Meta, Path, Type,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
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
    declared_generics: HashSet<Ident>,
    used_generics: HashSet<&'a Ident>,
    used_assocs: HashSet<&'a Path>,
}

impl<'a> GenericAnalysis<'a> {
    fn set_declared(&mut self, generics: &Generics) -> &mut Self {
        self.declared_generics = generics.type_params().map(|x| &x.ident).cloned().collect();
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
    fn from_generics(generics: &Generics, fields: &'a [FieldDef]) -> Self {
        let mut gens = Self::default();
        gens.set_declared(generics);
        fields.iter().for_each(|x| gens.analyse_use(x.ty));
        gens
    }
    fn mk_auto_where_clause(&self) -> Option<TokenStream> {
        if !self.used_assocs.is_empty() {
            let assocs = self.used_assocs.iter().map(|t| {
                quote! { #t : std::fmt::Debug }
            });
            Some(quote! { where #(#assocs),* })
        } else {
            None
        }
    }
    fn add_trait_bounds(&self, generics: &mut Generics) {
        for param in &mut generics.params {
            if let GenericParam::Type(ref mut type_param) = *param {
                if self.used_generics.contains(&type_param.ident) {
                    type_param.bounds.push(parse_quote!(std::fmt::Debug))
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

fn get_explicit_bounds(attrs: &[Attribute]) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let mut ans = Vec::new();
    for at in attrs.iter() {
        if at.path.is_ident("debug") {
            let meta = at.parse_meta()?;
            if let Meta::List(l) = &meta {
                for a in l.nested.iter() {
                    if let syn::NestedMeta::Meta(m) = a {
                        if let Meta::NameValue(nv) = m {
                            if nv.path.is_ident("bound") {
                                if let syn::Lit::Str(s) = &nv.lit {
                                    ans.push(s.value().parse()?);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(ans)
}

fn mk_derive(mut input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let explicit_bounds = get_explicit_bounds(&input.attrs)?;

    let name = &input.ident;
    let name_as_str = proc_macro2::Literal::string(&name.to_string());
    let fs = fields(&input.data, input.span())?;

    let field_uses = mk_debug_fields(&fs);

    let gens = GenericAnalysis::from_generics(&mut input.generics, &fs);
    if explicit_bounds.is_empty() {
        gens.add_trait_bounds(&mut input.generics);
    }
    let (impl_generics, ty_generics, _where_clause) = input.generics.split_for_impl();
    let where_clause = if !explicit_bounds.is_empty() {
        Some(quote! { where #(#explicit_bounds),* })
    } else {
        gens.mk_auto_where_clause()
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
