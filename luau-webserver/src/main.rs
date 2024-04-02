#[macro_use]
extern crate rocket;

use luau_lifter::decompile_bytecode;
use base64::prelude::*;

#[post("/decompile", data = "<bytecode>")]
async fn decompile(bytecode: String) -> String {
    let bytecode = BASE64_STANDARD.decode(bytecode).unwrap();
    decompile_bytecode(&bytecode, 1)
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![decompile])
}