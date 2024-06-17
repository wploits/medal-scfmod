extern crate console_error_panic_hook;

use worker::*;

#[event(fetch, respond_with_errors)]
pub async fn main(req: Request, env: Env, _ctx: Context) -> Result<Response> {
    console_error_panic_hook::set_once();

    let router = Router::new();
    router
        .get_async("/link", |req, _ctx| async move {
            Response::from_html(include_str!("static/link.html"))
        })
        .run(req, env)
        .await
}
