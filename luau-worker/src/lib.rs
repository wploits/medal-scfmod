extern crate console_error_panic_hook;

use base64::prelude::*;
use luau_lifter::decompile_bytecode;
use serde::{Deserialize, Serialize};
use worker::*;

const SELLIX_SECRET: &str = "G0WWbMPy4cMo8XJjQ0D237PiT7SUNwOzOPIxT4ju2qFUwqwCwasbUzB80JIbpuTG";
const SELLIX_PRODUCT_ID: &str = "6650c83ce5125";

#[derive(Serialize)]
struct LicensingCheck {
    product_id: String,
    key: String,
}

#[derive(Deserialize)]
struct LicenseData {}

#[derive(Deserialize)]
struct LicenseResponse {
    data: Option<LicenseData>,
}

#[event(fetch, respond_with_errors)]
pub async fn main(req: Request, env: Env, _ctx: worker::Context) -> Result<Response> {
    console_error_panic_hook::set_once();

    let router = Router::new();
    router
        .post_async("/decompile", |mut req, _ctx| async move {
            let hardcoded_keys: Vec<String> = vec![
                "MEDAL-JYPKXEDJGRHSTUMC".to_string(),
                "MEDAL-JVPTRXQQCWRXSKQY".to_string(),
                "MEDAL-ZGGZKVTHRLICJBWK".to_string(),
                "MEDAL-YRBAOLITKPFXKMRI".to_string(),
                "MEDAL-KJIVWRDEEDCCSMNI".to_string(),
                "MEDAL-YEGQGDZJKHZRPOSD".to_string(),
                "MEDAL-JHIFZJQDNJQCHVAE".to_string(),
                "MEDAL-AQXGCADHAHAWSPGS".to_string(),
                "MEDAL-JXWYLQLWYNBLALYN".to_string(),
                "MEDAL-AHSLAOKYLXONYCXO".to_string(),
                "MEDAL-SOLARA-TEST-QUI".to_string(),
            ];

            let license = req
                .headers()
                .get("Authorization")
                .unwrap_or_default()
                .expect("authorization header is required");

            let body = LicensingCheck {
                product_id: SELLIX_PRODUCT_ID.to_string(),
                key: license.clone(),
            };

            let json_body = serde_json::to_string(&body).unwrap();

            let mut headers = Headers::new();
            headers.set("Authorization", SELLIX_SECRET).unwrap();

            let request = Request::new_with_init(
                "https://dev.sellix.io/v1/products/licensing/check",
                &RequestInit {
                    method: Method::Post,
                    headers,
                    body: Some(json_body.into()),
                    ..RequestInit::default()
                },
            )?;

            let license_response: LicenseResponse =
                Fetch::Request(request).send().await?.json().await?;
            if license_response.data.is_none() && !hardcoded_keys.contains(&license) {
                return Response::error("invalid license", 403);
            }

            let encoded_bytecode = req.bytes().await?;
            match BASE64_STANDARD.decode(encoded_bytecode) {
                Ok(bytecode) => Response::ok(decompile_bytecode(&bytecode, 203)),
                Err(_) => Response::error("invalid bytecode", 400),
            }
        })
        .run(req, env)
        .await
}
