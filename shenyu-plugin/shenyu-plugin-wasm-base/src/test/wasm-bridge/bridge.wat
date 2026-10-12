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

;; Control words: input length at 0, result length at 4, argument ID at 8,
;; mode at 16, hook counts at 20/24, handler count at 28, sizing result at 32.
;; Arguments start at 4096 and the result starts at 1200000.
(module
  (import "shenyu" "get_args" (func $get_args (param i64 i64 i32) (result i32)))
  (import "shenyu" "put_result" (func $put_result (param i64 i64 i32) (result i32)))
  (memory (export "memory") 32)
  (data (i32.const 1536) "\22handle\22:\22rule-deny\22")
  (data (i32.const 1600) "{\22version\22:1,\22action\22:\22REJECT\22,\22status\22:403,\22body\22:\22configured denial\22}")
  ;; Mode 3 makes a real guest decision from the serialized rule handle.
  (func $denied (param $length i32) (result i32)
    (local $i i32)
    (local $j i32)
    (block $done
      (loop $outer
        (br_if $done (i32.gt_u (i32.add (local.get $i) (i32.const 20)) (local.get $length)))
        (local.set $j (i32.const 0))
        (block $mismatch
          (loop $inner
            (br_if $mismatch
              (i32.ne
                (i32.load8_u (i32.add (i32.const 4096) (i32.add (local.get $i) (local.get $j))))
                (i32.load8_u (i32.add (i32.const 1536) (local.get $j)))))
            (local.set $j (i32.add (local.get $j) (i32.const 1)))
            (if (i32.eq (local.get $j) (i32.const 20)) (then (return (i32.const 1))))
            (br $inner)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $outer)))
    (i32.const 0))
  (func $read (param $id i64)
    (local $size i32)
    (i64.store (i32.const 8) (local.get $id))
    (local.set $size
      (call $get_args (local.get $id) (i64.const 4096) (i32.const 0)))
    (i32.store (i32.const 32) (local.get $size))
    (i32.store (i32.const 0)
      (call $get_args (local.get $id) (i64.const 4096)
        (i32.sub (i32.const 0) (local.get $size)))))
  (func (export "doExecute") (param $id i64)
    (call $read (local.get $id))
    (if (i32.eq (i32.load (i32.const 16)) (i32.const 2))
      (then
        (drop (call $get_args (local.get $id) (i64.const 4294967296) (i32.const 1)))))
    (if (i32.and (i32.eq (i32.load (i32.const 16)) (i32.const 3))
          (call $denied (i32.load (i32.const 0))))
      (then
        (drop (call $put_result (local.get $id) (i64.const 1600) (i32.const 71))))
      (else
        (if (i32.ge_s (i32.load (i32.const 4)) (i32.const 0))
          (then
            (drop (call $put_result (local.get $id) (i64.const 1200000)
              (i32.load (i32.const 4))))))))
    (if (i32.eq (i32.load (i32.const 16)) (i32.const 1))
      (then unreachable)))
  (func (export "before") (param $id i64)
    (call $read (local.get $id))
    (i32.store (i32.const 20) (i32.add (i32.load (i32.const 20)) (i32.const 1))))
  (func (export "after") (param $id i64)
    (call $read (local.get $id))
    (i32.store (i32.const 24) (i32.add (i32.load (i32.const 24)) (i32.const 1))))
  ;; A configuration handler has separate guest state. Normal gateway cache
  ;; updates must still be visible in the request plugin's JSON payload.
  (func $handle (param i64)
    (i32.store (i32.const 28) (i32.add (i32.load (i32.const 28)) (i32.const 1))))
  (export "handlerPlugin" (func $handle))
  (export "handlerSelector" (func $handle))
  (export "handlerRule" (func $handle))
  (export "removePlugin" (func $handle))
  (export "removeSelector" (func $handle))
  (export "removeRule" (func $handle)))
