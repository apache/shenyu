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

pub use jni::errors::Error;
use jni::{errors::ErrorKind, JNIEnv};
use std::thread;

pub fn runtime_error(message: String) -> Error {
    Error::from_kind(ErrorKind::Msg(message))
}

#[derive(Debug)]
pub enum JOption<T> {
    Some(T),
    None,
}

impl<T> JOption<T> {
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            JOption::Some(result) => result,
            JOption::None => default,
        }
    }
}

pub fn joption_or_throw<T>(env: &JNIEnv, result: thread::Result<Result<T, Error>>) -> JOption<T> {
    match result {
        Ok(result) => match result {
            Ok(result) => JOption::Some(result),
            Err(error) => {
                if !env.exception_check().unwrap() {
                    env.throw_new("java/lang/RuntimeException", &error.to_string())
                        .expect("Cannot throw an `java/lang/RuntimeException` exception.");
                }

                JOption::None
            }
        },
        Err(ref error) => {
            env.throw_new("java/lang/RuntimeException", format!("{:?}", error))
                .expect("Cannot throw an `java/lang/RuntimeException` exception.");

            JOption::None
        }
    }
}
