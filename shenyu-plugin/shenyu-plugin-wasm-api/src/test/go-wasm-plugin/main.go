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

// Go WASM plugin for AbstractWasmPlugin.
// Exports execute, before, after — all (i64) -> ().
// Compile with: tinygo build -target wasm-unknown -opt=2 -no-debug -panic=trap -o plugin.wasm main.go

package main

import (
	"shenyu/wasmabi"
	"strconv"
	"unsafe"
)

//go:wasmexport execute
func execute(argId int64) {
	buf := make([]byte, 1024)
	wasmabi.Eprintln("go side-> buffer base address: " + strconv.FormatUint(uint64(uintptr(unsafe.Pointer(&buf[0]))), 10))
	input := wasmabi.GetArgs(argId, buf)
	wasmabi.Eprintln("go side-> GetArgs returned " + strconv.Itoa(len(input)) + ", recv:" + string(input))
	wasmabi.PutResult(argId, []byte("go result"))
}

//go:wasmexport before
func before(argId int64) {
	// Optional hook: called before request processing.
}

//go:wasmexport after
func after(argId int64) {
	// Optional hook: called after request processing.
}

// main is required by TinyGo's WASI target but is never called.
func main() {}
