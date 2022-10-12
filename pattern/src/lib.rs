use proc_macro::TokenStream;

#[proc_macro]
pub fn pattern(item: TokenStream) -> TokenStream {
    "fn answer() -> u32 { 42 }".parse().unwrap()
}
