use pattern::pattern;

fn main() {
    pattern!(
        @body
        @exit
        * $ -> entry
        - entry -> body &body_edge
        - entry -> exit
        - body -> exit
    );
}
