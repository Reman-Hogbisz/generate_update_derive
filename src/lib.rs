use proc_macro::{self, TokenStream};
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, DeriveInput, FieldsNamed, Lit, Meta, NestedMeta,
    Token,
};

fn parse_update_ignored_fields(attr: &syn::Attribute) -> Vec<String> {
    attr.parse_args_with(Punctuated::<NestedMeta, Token![,]>::parse_terminated)
        .unwrap_or_else(|err| {
            panic!("update_ignored_fields failed to parse arguments with error: {err}")
        })
        .iter()
        .enumerate()
        .map(|(index, nested)| match nested {
            NestedMeta::Meta(Meta::Path(path)) => path
                .get_ident()
                .expect(&format!(
                    "update_ignored_fields failed to get identifier off path at index {index}"
                ))
                .to_string(),
            NestedMeta::Lit(Lit::Str(s)) => s.value(),
            _ => panic!("update_ignored_fields must be a list of identifiers or strings"),
        })
        .collect()
}

#[proc_macro_derive(CreateUpdate, attributes(update_ignored_fields, sql_path, table_name))]
pub fn create_update(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident, data, attrs, ..
    } = parse_macro_input!(input);

    let table_name_attr = match attrs.iter().find(|attr| attr.path.is_ident("table_name")) {
        Some(attr) => attr,
        None => panic!("derive(CreateUpdate) requires a table_name attribute"),
    };

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
        Some(attr) => parse_update_ignored_fields(attr),
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

#[cfg(test)]
mod tests {
    use super::parse_update_ignored_fields;
    use syn::parse_quote;

    #[test]
    fn parses_identifier_list() {
        let attr: syn::Attribute =
            parse_quote!(#[update_ignored_fields(post_id, created_at, updated_at)]);
        assert_eq!(
            parse_update_ignored_fields(&attr),
            vec!["post_id", "created_at", "updated_at"]
        );
    }

    #[test]
    fn parses_string_list() {
        let attr: syn::Attribute = parse_quote!(#[update_ignored_fields("post_id", "created_at")]);
        assert_eq!(
            parse_update_ignored_fields(&attr),
            vec!["post_id", "created_at"]
        );
    }

    #[test]
    fn parses_identifier_individual() {
        let attr: syn::Attribute = parse_quote!(#[update_ignored_fields(post_id)]);
        assert_eq!(parse_update_ignored_fields(&attr), vec!["post_id"]);
    }

    #[test]
    fn parses_string_individual() {
        let attr: syn::Attribute = parse_quote!(#[update_ignored_fields("post_id")]);
        assert_eq!(parse_update_ignored_fields(&attr), vec!["post_id"]);
    }
}
