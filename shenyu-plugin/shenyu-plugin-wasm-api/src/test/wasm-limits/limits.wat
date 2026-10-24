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
  (import "test" "block" (func $block))
  (memory (export "memory") 1 4)
  (func (export "spin")
    (i32.store (i32.const 0) (i32.const 99))
    (loop $forever (br $forever)))
  (func (export "block") (call $block))
  (func (export "value") (param i64) (result i64) (local.get 0))
  (func (export "read") (result i32) (i32.load (i32.const 0)))
  (func (export "grow") (param i32) (result i32) (memory.grow (local.get 0))))

