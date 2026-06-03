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

// Package wasmabi provides helper functions for communicating with the Java
// ShenYu host via shared memory. TinyGo WASM plugins should use GetArgs and
// PutResult instead of calling the raw host imports directly.
package wasmabi

import "unsafe"

//go:wasmimport shenyu get_args
func getArgsRaw(argId int64, addr int64, len int32) int32

//go:wasmimport shenyu put_result
func putResultRaw(argId int64, addr int64, len int32) int32

// GetArgs reads serialized data from the Java host via shared memory.
// The caller provides a pre-allocated buffer; the returned slice is a
// sub-slice of buf containing the actual data that was written.
func GetArgs(argId int64, buf []byte) []byte {
	ptr := unsafe.Pointer(&buf[0])
	n := getArgsRaw(argId, int64(uintptr(ptr)), int32(len(buf)))
	return buf[:n]
}

// PutResult writes serialized data back to the Java host via shared memory.
func PutResult(argId int64, data []byte) {
	ptr := unsafe.Pointer(&data[0])
	_ = putResultRaw(argId, int64(uintptr(ptr)), int32(len(data)))
}

//go:wasmimport wasi_snapshot_preview1 fd_write
func fdWrite(fd int32, iovs int32, iovsLen int32, nwrittenPtr int32) int32

// Eprintln prints a string to stderr (fd 2) via WASI fd_write.
// This is the Go equivalent of Rust's eprintln!.
func Eprintln(msg string) {
	msg += "\n"
	ptr := unsafe.Pointer(unsafe.StringData(msg))
	var nwritten uint32
	buf := [2]uint32{uint32(uintptr(ptr)), uint32(len(msg))}
	bufPtr := unsafe.Pointer(&buf[0])
	nwrittenPtr := unsafe.Pointer(&nwritten)
	_ = fdWrite(2, int32(uintptr(bufPtr)), 1, int32(uintptr(nwrittenPtr)))
}
