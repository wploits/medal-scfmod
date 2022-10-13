use proc_macro::TokenStream;

#[proc_macro]
pub fn pattern(input: TokenStream) -> TokenStream {
    dbg!(input);
    "fn answer() -> u32 { 42 }".parse().unwrap()
}
