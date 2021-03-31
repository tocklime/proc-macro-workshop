use std::convert::TryFrom;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Literal, Span};
use quote::{quote, quote_spanned};
use syn::{self, parse_macro_input, spanned::Spanned, Data, DeriveInput, Lit, Type};
use if_chain::if_chain;

struct FieldDef<'a> {
    name: &'a Ident,
    ty: &'a Type,
    each_fn: Option<String>,
}
impl<'a> TryFrom<&'a syn::Field> for FieldDef<'a> {
    type Error = syn::Error;

    fn try_from(f: &'a syn::Field) -> Result<Self, Self::Error> {
        let name = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        let mut each_fn = None;
        let builder_attrs = f
            .attrs
            .iter()
            .filter(|a| a.path.is_ident("builder"))
            .collect::<Vec<_>>();
        for at in builder_attrs {
            let meta = at.parse_meta()?;
            if_chain! {
                if let syn::Meta::List(x) = &meta;
                if x.nested.len() == 1;
                if let syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) = &x.nested[0];
                if nv.path.is_ident("each");
                if let Lit::Str(s) = &nv.lit;
                then {
                    each_fn = Some(s.value());
                } else {
                    return Err(syn::Error::new_spanned(
                        meta,
                        "expected `builder(each = \"...\")`",
                    ));
                }
            };
        }
        Ok(Self { name, ty, each_fn })
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
fn unwrap_singleton_generic<'a>(ty: &'a Type, outer_name: &str) -> Option<&'a Type> {
    if_chain! {
        if let Type::Path(tp) = ty;
        if tp.qself.is_none()
            && tp.path.segments.len() == 1
            && tp.path.segments[0].ident.to_string() == outer_name;
        if let syn::PathArguments::AngleBracketed(ab) = &tp.path.segments[0].arguments;
        if ab.args.len() == 1;
        if let syn::GenericArgument::Type(ty) = &ab.args[0];
        then {
            Some(ty)
        } else {
            None
        }
    }
}

fn mk_decls(fields: &[FieldDef]) -> proc_macro2::TokenStream {
    let list = fields.iter().map(|FieldDef { name, ty, each_fn }| {
        if each_fn.is_some() {
            quote_spanned! {name.span()=> #name : #ty }
        } else {
            let inner = unwrap_singleton_generic(ty, "Option").unwrap_or(ty);
            quote_spanned! {name.span()=> #name : std::option::Option<#inner> }
        }
    });
    quote! { #(#list),* }
}
fn mk_set_nones(fields: &[FieldDef]) -> proc_macro2::TokenStream {
    let list = fields.iter().map(|FieldDef { name, each_fn, .. }| {
        if each_fn.is_some() {
            quote! { #name : std::vec::Vec::new() }
        } else {
            quote! { #name : std::option::Option::None }
        }
    });
    quote! {#(#list),*}
}
fn mk_builds(fields: &[FieldDef]) -> proc_macro2::TokenStream {
    let list = fields.iter().map(|FieldDef { name, ty, each_fn }| {
        let err_str = format!("Field not set before build() call: {}", name);
        let err_lit = Lit::new(Literal::string(&err_str));
        if each_fn.is_some() || unwrap_singleton_generic(ty, "Option").is_some() {
            quote! { #name : self.#name.clone() }
        } else {
            quote! { #name : self.#name.clone().ok_or(#err_lit)? }
        }
    });
    quote! {#(#list),*}
}
fn mk_setters(fields: &[FieldDef]) -> syn::Result<proc_macro2::TokenStream> {
    let list = fields
        .iter()
        .map(
            |FieldDef { name, ty, each_fn }| -> syn::Result<proc_macro2::TokenStream> {
                let inner = unwrap_singleton_generic(ty, "Option").unwrap_or(ty);
                Ok(match &each_fn {
                    Some(e) => {
                        let inner: Result<&Type, syn::Error> = unwrap_singleton_generic(ty, "Vec")
                            .ok_or_else(|| syn::Error::new(ty.span(), "Expected Vec type here"));
                        let inner = inner?;
                        let each_ident = Ident::new(e, name.span());
                        if &name.to_string() == e {
                            //each_fn, colliding name
                            quote! {
                                fn #each_ident(&mut self, #name: #inner) -> &mut Self {
                                    self.#name.push(#name);
                                    self
                                }
                            }
                        } else {
                            //each_fn, different name
                            quote! {
                                fn #each_ident(&mut self, #name: #inner) -> &mut Self {
                                    self.#name.push(#name);
                                    self
                                }
                                fn #name(&mut self, #name: #ty) -> &mut Self {
                                    self.#name = #name;
                                    self
                                }
                            }
                        }
                    }
                    None => {
                        quote! {
                            fn #name(&mut self, #name: #inner) -> &mut Self {
                                self.#name = Some(#name);
                                self
                            }
                        }
                    }
                })
            },
        )
        .collect::<Result<Vec<_>, _>>()?;
    Ok(quote! {#(#list)*})
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    mk_builder(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn mk_builder(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = &input.ident;
    let builder = Ident::new(&(name.to_string() + "Builder"), name.span());
    let fields = fields(&input.data, input.span())?;
    let fields_types = mk_decls(&fields);
    let fields_set_none = mk_set_nones(&fields);
    let fields_setters = mk_setters(&fields)?;
    let fields_set_from_builder = mk_builds(&fields);

    Ok(quote! {
        pub struct #builder {
            #fields_types
        }
        impl #builder {
            #fields_setters
            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#name {
                    #fields_set_from_builder
                })
            }
        }
        impl #name {
            pub fn builder() -> #builder {
                #builder {
                    #fields_set_none
                }
            }
        }
    })
}
