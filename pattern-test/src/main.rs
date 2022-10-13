use pattern::pattern;

fn main() {
    pattern!(
        @entry
        @body
        @exit
        * $ -> entry
        - entry -> body &body_edge
        - entry -> exit
        - body -> exit
    );
}
