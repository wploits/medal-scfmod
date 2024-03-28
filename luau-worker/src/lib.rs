use luau_lifter::decompile_bytecode;
use worker::*;

extern crate console_error_panic_hook;

#[event(fetch, respond_with_errors)]
pub async fn main(req: Request, env: Env, _ctx: worker::Context) -> Result<Response> {
    console_error_panic_hook::set_once();

    let router = Router::new();

    router
        .get_async("/decompile", |mut req, ctx| async move {
            let bytecode = req.form_data().await?.get("bytecode").unwrap();
            match bytecode {
                FormEntry::File(file) => {
                    let content = unsafe { String::from_utf8_unchecked(file.bytes().await?) };
                    Response::ok(decompile_bytecode(&content, 1))
                }
                FormEntry::Field(_) => Response::error("expected bytecode file", 400),
            }
        })
        .run(req, env)
        .await
}
