use anyhow::Result;
use luau_lifter::decompile_bytecode;
use warp::Filter;
use base64::{Engine as _, engine::general_purpose};
use bytes::Bytes;

#[tokio::main]
async fn main() -> Result<()> {
    let decompile = warp::post()
        .and(warp::path("decompile"))
        .and(warp::body::bytes())
        .map(|body: Bytes| {
            let encoded_bytecode = body.to_vec();
            match general_purpose::STANDARD.decode(&encoded_bytecode) {
                Ok(bytecode) => {
                    let mut decompiled_code = String::from("-- Decompiled with wp-. engine :)\n\n");
                    decompiled_code.push_str(&decompile_bytecode(&bytecode, 203));
                    warp::reply::with_status(decompiled_code, warp::http::StatusCode::OK)
                }
                Err(_) => {
                    warp::reply::with_status("Invalid bytecode".to_string(), warp::http::StatusCode::BAD_REQUEST)
                }
            }
        });

    println!("Listening on http://127.0.0.1:9002");
    warp::serve(decompile).run(([127, 0, 0, 1], 9002)).await;

    Ok(())
}
