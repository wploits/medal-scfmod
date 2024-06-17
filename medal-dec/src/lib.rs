extern crate console_error_panic_hook;

use base64::prelude::*;
use luau_lifter::decompile_bytecode;
use serde::{Deserialize, Serialize};
use worker::*;

#[derive(Deserialize)]
struct DbLicense {}

async fn check_license(env: &Env, license_key: String) -> bool {
    let d1 = env.d1("medal").expect("failed to connect to database");
    let statement = d1
        .prepare("SELECT * FROM licenses WHERE key = ? AND expiration > ? LIMIT 1")
        .bind(&[license_key.into(), (chrono::Utc::now().timestamp() as i32).into()])
        .unwrap();
    !statement
        .run()
        .await
        .unwrap()
        .results::<DbLicense>()
        .unwrap()
        .is_empty()
}

#[event(fetch, respond_with_errors)]
pub async fn main(req: Request, env: Env, _ctx: Context) -> Result<Response> {
    console_error_panic_hook::set_once();

    let router = Router::new();
    router
        .get_async("/link", |req, _ctx| async move {
            let index = include_str!("index.html");
        })
        .post_async("/", |mut req, ctx| async move {
            let license_key = req
                .headers()
                .get("license")
                .unwrap_or_default()
                .expect("license header is required");
            if !check_license(&ctx.env, license_key).await {
                return Response::error("invalid license", 403);
            }

            Response::ok("ok")
        })
        .run(req, env)
        .await
}
