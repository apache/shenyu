(;
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements.  See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
;)

;; Pure echo guest: put_result returns exactly the bytes get_args delivered,
;; so each call's result is bound to its own request payload.
(module
  (import "shenyu" "get_args" (func $get_args (param i64 i64 i32) (result i32)))
  (import "shenyu" "put_result" (func $put_result (param i64 i64 i32) (result i32)))
  (memory (export "memory") 1)
  (func (export "execute") (param $argId i64)
    (local $len i32)
    (local.set $len (call $get_args (local.get $argId) (i64.const 1024) (i32.const 4096)))
    (drop (call $put_result (local.get $argId) (i64.const 1024) (local.get $len)))))
