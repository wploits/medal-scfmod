fn main() {
    let file_name = std::env::args().nth(1).expect("expected exactly one file");
    let bytecode = std::fs::read(file_name).expect("failed to read file");
    println!("{}", luau_lifter::decompile_bytecode(&bytecode, 203));
}
