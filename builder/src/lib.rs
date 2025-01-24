use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Expr, Fields, GenericArgument, Ident, Lit, PathArguments, Type};

#[proc_macro_derive(Builder, attributes(builder))]
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

    let mut builder_each_setters = Vec::new();
    let mut builder_each_setter_names = Vec::new();

    let builder_fields = fields.named.iter().map(|field| {
        let field_name = &field.ident;
        let field_type = &field.ty;

        for attr in field.attrs.iter().filter(|attr| attr.path().is_ident("builder")) {
            let expr: Expr = attr.parse_args().unwrap();
            if let Expr::Assign(expr) = expr {
                if let Expr::Path(left) = *expr.left {
                    if left.path.is_ident("each") {
                        let Expr::Lit(right) = *expr.right else {
                            panic!("Expected a literal value on the right-hand side");
                        };

                        let each_setter_name = if let Lit::Str(lit_str) = right.lit {
                            syn::Ident::new(&lit_str.value(), lit_str.span())
                        } else {
                            panic!("Expected a string literal on the right-hand side");
                        };

                        let vec = match field_type {
                            Type::Path(type_path) => type_path.path.segments.last().unwrap(),
                            _ => panic!("Expected a field of type Vec when using #[each]"),
                        };

                        if vec.ident != "Vec" {
                            panic!("Expected a field of type Vec when using #[each]");
                        }

                        let PathArguments::AngleBracketed(ref argument) = vec.arguments else {
                            panic!("Expected angle-bracketed arguments for Vec.");
                        };

                        let GenericArgument::Type(arg_type) = argument.args.first().unwrap().to_owned() else {
                            panic!("Expected a type argument for Option.");
                        };

                        builder_each_setter_names.push(each_setter_name.to_string());

                        builder_each_setters.push(quote! {
                            fn #each_setter_name(&mut self, #each_setter_name: #arg_type) -> &mut Self {
                                match &mut self.#field_name {
                                    Some(#field_name) => #field_name.push(#each_setter_name),
                                    None => self.#field_name = Some(vec![#each_setter_name]),
                                }
                                self
                            }
                        });
                    }
                }
            }
        }

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

    let builder_setters = fields.named.iter().map(|field| {
        let field_name = &field.ident;
        let field_type = &field.ty;

        if let Type::Path(type_path) = field_type {
            let option = type_path.path.segments.last().unwrap();
            if option.ident == "Option" {
                let PathArguments::AngleBracketed(ref argument) = option.arguments else {
                    panic!("Expected angle-bracketed arguments for Option.");
                };

                let GenericArgument::Type(arg_type) = argument.args.first().unwrap().to_owned() else {
                    panic!("Expected a type argument for Option.");
                };

                return quote! {
                    fn #field_name(&mut self, #field_name: #arg_type) -> &mut Self {
                        self.#field_name = Some(#field_name);
                        self
                    }
                };
            }
        }

        quote! {
            fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                self.#field_name = Some(#field_name);
                self
            }
        }
    });

    let build_exprs = fields.named.iter().map(|field| {
        let field_name = &field.ident;
        let field_type = &field.ty;

        if let Type::Path(type_path) = field_type {
            if type_path.path.segments.last().unwrap().ident == "Option" {
                return quote! {
                    #field_name: self.#field_name.take()
                };
            }
        }

        quote! {
            #field_name: self.#field_name.take().ok_or("Missing #field_name")?
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

        impl #builder_name {
            #(#builder_setters)*

            #(#builder_each_setters)*

            pub fn build(&mut self) -> Result<Command, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_exprs,)*
                })
            }
        }
    };

    TokenStream::from(expanded)
}
