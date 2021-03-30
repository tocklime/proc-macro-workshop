use proc_macro::TokenStream;
use proc_macro2::{Ident, Literal};
use quote::{quote, quote_spanned};
use syn::{self, parse_macro_input, Data, DeriveInput, Lit, Type};

struct FieldDef<'a> {
    name: &'a Ident,
    ty: &'a Type,
    //each_fn: Option<&'a str>
}
fn fields(data: &Data) -> Vec<FieldDef> {
    match data {
        Data::Enum(_) => panic!("Can't make builder for enum types"),
        Data::Union(_) => panic!("Can't make builder for union types"),
        Data::Struct(s) => match s.fields {
            syn::Fields::Unnamed(_) => panic!("Can't make builder for unnamed fields"),
            syn::Fields::Unit => panic!("Can't make builder for unit type"),
            syn::Fields::Named(ref fields) => fields
                .named
                .iter()
                .map(|f| {
                    let name = f.ident.as_ref().unwrap();
                    let ty = &f.ty;
                    FieldDef { name, ty }
                })
                .collect(),
        },
    }
}
fn unwrap_option(ty: &Type) -> Option<&Type> {
    if let Type::Path(tp) = ty {
        if tp.qself.is_none()
            && tp.path.segments.len() == 1
            && tp.path.segments[0].ident.to_string() == "Option"
        {
            if let syn::PathArguments::AngleBracketed(ab) = &tp.path.segments[0].arguments {
                if ab.args.len() == 1 {
                    if let syn::GenericArgument::Type(ty) = &ab.args[0] {
                        return Some(ty);
                    }
                }
            }
        }
    }
    None
}

fn mk_decls(fields: &[FieldDef]) -> proc_macro2::TokenStream {
    let list = fields.iter().map(|&FieldDef { name, ty }| {
        let inner = unwrap_option(ty).unwrap_or(ty);
        quote_spanned! {name.span()=> #name : Option<#inner> }
    });
    quote! { #(#list),* }
}
fn mk_set_nones(fields: &[FieldDef]) -> proc_macro2::TokenStream {
    let list = fields
        .iter()
        .map(|&FieldDef { name, .. }| quote! { #name : None });
    quote! {#(#list),*}
}
fn mk_builds(fields: &[FieldDef]) -> proc_macro2::TokenStream {
    let list = fields.iter().map(|&FieldDef { name, ty }| {
        let err_str = format!("Field not set before build() call: {}", name);
        let err_lit = Lit::new(Literal::string(&err_str));
        if unwrap_option(ty).is_some() {
            quote! { #name : self.#name.clone() }
        } else {
            quote! { #name : self.#name.clone().ok_or(#err_lit)? }
        }
    });
    quote! {#(#list),*}
}
fn mk_setters(fields: &[FieldDef]) -> proc_macro2::TokenStream {
    let list = fields.iter().map(|&FieldDef { name, ty }| {
        let inner = unwrap_option(ty).unwrap_or(ty);
        quote! {
            fn #name(&mut self, #name: #inner) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });
    quote! {#(#list)*}
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder = Ident::new(&(name.to_string() + "Builder"), name.span());
    let fields = fields(&input.data);
    let fields_types = mk_decls(&fields);
    let fields_set_none = mk_set_nones(&fields);
    let fields_setters = mk_setters(&fields);
    let fields_set_from_builder = mk_builds(&fields);

    let expanded = quote! {
        pub struct #builder {
            #fields_types
        }
        impl #builder {
            #fields_setters
            pub fn build(&mut self) -> Result<Command, Box<dyn std::error::Error>> {
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
    };
    proc_macro::TokenStream::from(expanded)
}
