;; Licensed to the Apache Software Foundation (ASF) under one or more
;; contributor license agreements. See the NOTICE file distributed with
;; this work for additional information regarding copyright ownership.
;; The ASF licenses this file to You under the Apache License, Version 2.0
;; (the "License"); you may not use this file except in compliance with
;; the License. You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Uses the bridge test's input/result layout, without optional hook exports.
(module
  (import "shenyu" "get_args" (func $get_args (param i64 i64 i32) (result i32)))
  (import "shenyu" "put_result" (func $put_result (param i64 i64 i32) (result i32)))
  (memory (export "memory") 32)
  (func (export "doExecute") (param $id i64)
    (local $size i32)
    (local.set $size (call $get_args (local.get $id) (i64.const 0) (i32.const 0)))
    (i32.store (i32.const 0)
      (call $get_args (local.get $id) (i64.const 4096)
        (i32.sub (i32.const 0) (local.get $size))))
    (drop (call $put_result (local.get $id) (i64.const 1200000) (i32.load (i32.const 4))))))
