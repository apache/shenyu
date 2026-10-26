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

(module
  (import "shenyu" "get_args" (func $get_args (param i64 i64 i32) (result i32)))
  (import "shenyu" "put_result" (func $put_result (param i64 i64 i32) (result i32)))
  (memory (export "memory") 1)
  (func $ack (param $id i64) (param $event i32)
    (local $length i32)
    (local.set $length (call $get_args (local.get $id) (i64.const 16) (i32.const 65520)))
    (if (i32.le_s (local.get $length) (i32.const 0)) (then unreachable))
    (i32.store (i32.const 12) (local.get $event))
    (if (call $put_result (local.get $id) (i64.const 12)
          (i32.add (local.get $length) (i32.const 4))) (then unreachable)))
  (func (export "handlerPlugin") (param $id i64) (call $ack (local.get $id) (i32.const 0)))
  (func (export "handlerSelector") (param $id i64) (call $ack (local.get $id) (i32.const 1)))
  (func (export "handlerRule") (param $id i64) (call $ack (local.get $id) (i32.const 2)))
  (func (export "removePlugin") (param $id i64) (call $ack (local.get $id) (i32.const 3)))
  (func (export "removeSelector") (param $id i64) (call $ack (local.get $id) (i32.const 4)))
  (func (export "removeRule") (param $id i64) (call $ack (local.get $id) (i32.const 5))))
