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
