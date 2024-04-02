extern crate console_error_panic_hook;

use worker::*;
use luau_lifter::decompile_bytecode;

#[event(fetch, respond_with_errors)]
pub async fn main(req: Request, env: Env, _ctx: worker::Context) -> Result<Response> {
    console_error_panic_hook::set_once();

    let router = Router::new();

    router
        .get_async("/decompile", |mut req, _ctx| async move {
            let bytecode = req.form_data().await?.get("bytecode").unwrap();
            match bytecode {
                FormEntry::File(file) => {
                    Response::ok(decompile_bytecode(&file.bytes().await?, 1))
                }
                FormEntry::Field(_) => Response::error("expected bytecode file", 400),
            }
        })
        .run(req, env)
        .await
}
