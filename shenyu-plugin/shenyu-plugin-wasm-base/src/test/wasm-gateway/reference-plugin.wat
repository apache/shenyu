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

  ;; Return the address of the length prefix of field n in the request frame.
  (func $field (param $n i32) (result i32)
    (local $p i32)
    (local.set $p (i32.const 20))
    (block $done
      (loop $next
        (br_if $done (i32.eqz (local.get $n)))
        (local.set $p (i32.add (local.get $p)
          (i32.add (i32.const 4) (i32.load (local.get $p)))))
        (local.set $n (i32.sub (local.get $n) (i32.const 1)))
        (br $next)))
    (local.get $p))

  ;; Compare two length-prefixed UTF-8 fields (exact equality or prefix).
  (func $matches (param $a i32) (param $b i32) (param $prefix i32) (result i32)
    (local $length i32) (local $i i32)
    (local.set $length (i32.load (local.get $a)))
    (if (i32.gt_u (local.get $length) (i32.load (local.get $b)))
      (then (return (i32.const 0))))
    (if (i32.and (i32.eqz (local.get $prefix))
          (i32.ne (local.get $length) (i32.load (local.get $b))))
      (then (return (i32.const 0))))
    (block $done
      (loop $next
        (br_if $done (i32.eq (local.get $i) (local.get $length)))
        (if (i32.ne
              (i32.load8_u (i32.add (i32.add (local.get $a) (i32.const 4)) (local.get $i)))
              (i32.load8_u (i32.add (i32.add (local.get $b) (i32.const 4)) (local.get $i))))
          (then (return (i32.const 0))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $next)))
    (i32.const 1))

  (func (export "doExecute") (param $id i64)
    (local $length i32) (local $allow i32)
    ;; Deliberately exercise the reference adapter's no-partial-write contract.
    (if (i32.ne (call $get_args (local.get $id) (i64.const 16) (i32.const 1)) (i32.const -1))
      (then unreachable))
    (local.set $length (call $get_args (local.get $id) (i64.const 16) (i32.const 65520)))
    (if (i32.le_s (local.get $length) (i32.const 0)) (then unreachable))
    (if (i32.ne (i32.load (i32.const 16)) (i32.const 1)) (then unreachable))
    ;; Guest policy: request token equals rule handle AND selector handle prefixes path.
    (local.set $allow (i32.and
      (call $matches (call $field (i32.const 3)) (call $field (i32.const 7)) (i32.const 0))
      (call $matches (call $field (i32.const 1)) (call $field (i32.const 5)) (i32.const 1))))
    ;; Return the decision followed by the original frame, proving what the guest read.
    (i32.store (i32.const 12) (i32.eqz (local.get $allow)))
    (if (call $put_result (local.get $id) (i64.const 12)
          (i32.add (local.get $length) (i32.const 4))) (then unreachable)))
)
