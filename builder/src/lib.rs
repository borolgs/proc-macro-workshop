use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), name.span());

    let Data::Struct(data) = input.data else {
        panic!("The `Builder` macro can only be used with structs. Found a different data type.");
    };
    let Fields::Named(fields) = data.fields else {
        panic!("The `Builder` macro requires structs with named fields. Found a struct without named fields.");
    };

    let builder_fields = fields.named.iter().map(|field| {
        let field_name = &field.ident;
        let field_type = &field.ty;

        if let Type::Path(type_path) = field_type {
            if type_path.path.segments.last().unwrap().ident == "Option" {
                return quote! {
                    #field_name: #field_type
                };
            }
        }

        quote! {
            #field_name: Option<#field_type>
        }
    });

    let builder_fields_inits = fields.named.iter().map(|field| {
        let field_name = &field.ident;
        quote! {
            #field_name: None
        }
    });

    let expanded = quote! {
        // ...
        impl #name {
            //

            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_fields_inits,)*
                }
            }
        }

        pub struct #builder_name {
            #(#builder_fields,)*
        }
    };

    TokenStream::from(expanded)
}
