use proc_macro::{self, TokenStream};
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use syn::{parse_macro_input, DeriveInput, FieldsNamed, Meta};

#[proc_macro_derive(CreateUpdate, attributes(update_ignored_fields, sql_path, diesel))]
pub fn create_update(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident, data, attrs, ..
    } = parse_macro_input!(input);

    let diesel_attrs: Vec<&syn::Attribute> = attrs
        .iter()
        .filter(|attr| attr.path.is_ident("diesel"))
        .collect();

    assert!(
        !diesel_attrs.is_empty(),
        "derive(CreateUpdate) requires a diesel(table_name = \"...\") attribute (diesel attrs is empty)"
    );

    let table_name_attr = diesel_attrs.into_iter().find(|attr| {
        let tokens = attr.to_token_stream().into_iter().collect::<Vec<_>>();
        tokens
            .iter()
            .any(|token| token.to_string().contains("table_name"))
    });

    assert!(
        table_name_attr.is_some(),
        "derive(CreateUpdate) requires a diesel(table_name = \"...\") attribute (no table_name attr found)"
    );

    let table_name_attr = table_name_attr.unwrap(); // Safety: We just checked that it is some

    let sql_path_attribute = attrs
        .iter()
        .find(|attr| attr.path.is_ident("sql_path"))
        .expect("derive(CreateUpdate) requires a #[sql_path(...)] attribute with the path to the schema from diesel");

    let sql_table = if let syn::Meta::List(list) = sql_path_attribute
        .parse_meta()
        .expect("Failed to parse metadata of sql_path attribute")
    {
        list.nested.iter().find_map(|nested| {
            if let syn::NestedMeta::Meta(syn::Meta::Path(path)) = nested {
                Some(path.clone())
            } else {
                None
            }
        })
    } else {
        None
    };

    if sql_table.is_none() {
        panic!("derive(CreateUpdate) requires a sql_path attribute");
    }

    let update_ignored_fields_attr = attrs
        .iter()
        .find(|attr| attr.path.is_ident("update_ignored_fields"));

    let update_ignored_field_names: Vec<String> = match update_ignored_fields_attr {
        Some(attr) => match attr.parse_args() {
            Ok(Meta::List(list)) => list
                .nested
                .iter()
                .map(|nested| match nested {
                    syn::NestedMeta::Meta(Meta::Path(path)) => path
                        .get_ident()
                        .expect("update_ignored_fields must be a list of identifiers")
                        .to_string(),
                    _ => panic!("update_ignored_fields must be a list of identifiers"),
                })
                .collect(),
            _ => panic!("update_ignored_fields must be a list of identifiers"),
        },
        None => vec![
            "created_at".to_string(),
            "updated_at".to_string(),
            "id".to_string(),
        ],
    };

    let struct_token = match data {
        syn::Data::Struct(s) => s,
        _ => panic!("derive(CreateUpdate) only supports structs"),
    };

    let fields = match struct_token.fields {
        syn::Fields::Named(FieldsNamed { named, .. }) => named,
        _ => panic!("derive(CreateUpdate) only supports named fields"),
    };

    let (idents, types): (Vec<_>, Vec<_>) = fields
        .iter()
        .filter_map(|f| match f.ident {
            Some(ref i) => Some((i, &f.ty)),
            None => None,
        })
        .unzip();

    let mut optional_field_declarations = TokenStream2::default();

    idents
        .into_iter()
        .zip(types.into_iter())
        .for_each(|(field, ftype)| {
            if update_ignored_field_names.contains(&field.to_string()) {
                return;
            }

            optional_field_declarations.extend(quote! {
                pub #field: Option<#ftype>,
            });
        });

    let struct_name = Ident::new(&format!("Updatable{}", ident), Span::call_site());

    let output = quote! {

        use crate::util::*;
        use crate::db_connection::*;
        use diesel::prelude::*;

        #[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Insertable, AsChangeset, TS, Default)]
        #[ts(export)]
        #[diesel(treat_none_as_null = false)]
        #table_name_attr
        pub struct #struct_name {
            #optional_field_declarations
        }
    };

    output.into()
}
