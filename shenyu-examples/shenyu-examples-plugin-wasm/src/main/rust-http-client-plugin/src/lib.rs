/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
use minreq::{Method, Request};
use serde::{Deserialize, Serialize};
use std::io::{Error, ErrorKind};

#[link(wasm_import_module = "shenyu")]
extern "C" {
    fn get_args(arg_id: i64, addr: i64, len: i32) -> i32;

    fn put_result(arg_id: i64, addr: i64, len: i32);
}

#[derive(Serialize, Deserialize)]
struct Req {
    method: String,
    uri: String,
    body: Option<String>,
}

/// # Safety
/// This function is unsafe because it read/write data from java.
#[no_mangle]
pub unsafe extern "C" fn execute(arg_id: i64) {
    let mut buf = [0u8; 256 * 1024];
    let buf_ptr = buf.as_mut_ptr() as i64;
    // get arg from java
    let len = get_args(arg_id, buf_ptr, buf.len() as i32);
    let java_arg =
        std::str::from_utf8(&buf[..len as usize]).expect("java_arg is not a utf8 string");
    eprintln!("rust side-> recv:{}", java_arg);

    // pass rust result to java
    let rust_result = request(java_arg).unwrap_or_else(|_| r#"{"message": "request fail"}"#.into());
    let result_ptr = rust_result.as_ptr() as i64;
    put_result(arg_id, result_ptr, rust_result.len() as i32);
}

fn request(json: &str) -> std::io::Result<String> {
    let request: Req = serde_json::from_str(json)?;
    let mut builder = Request::new(Method::Custom(request.method), request.uri);
    // miss header, but this is just a example
    if let Some(body) = request.body {
        builder = builder.with_body(body);
    }
    String::from_utf8(
        builder
            .send()
            .map_err(|e| Error::new(ErrorKind::Other, e))?
            .as_bytes()
            .into(),
    )
    .map_err(|e| Error::new(ErrorKind::Other, e))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_request() {
        let json = r#"{"method":"GET","uri":"http://localhost:8189/order/findById?id=123"}"#;
        let result = request(json);
        assert!(result.is_ok());
        println!("{}", result.expect("request failed"));
    }
}
