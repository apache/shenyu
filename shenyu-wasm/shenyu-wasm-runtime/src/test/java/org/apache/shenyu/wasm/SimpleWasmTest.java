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

package org.apache.shenyu.wasm;

import java.io.IOException;
import java.net.URISyntaxException;
import org.junit.Assert;
import org.junit.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class SimpleWasmTest {
    @Test
    public void test() throws IOException, URISyntaxException {
        // `simple.wasm` is located at `tests/resources/`.
        Path wasmPath = Paths.get(this.getClass().getClassLoader().getResource("simple.wasm").toURI());
        
        // Reads the WebAssembly module as bytes.
        byte[] wasmBytes = Files.readAllBytes(wasmPath);
        
        // Instantiates the WebAssembly module.
        Instance instance = new Instance(wasmBytes);
        
        // Calls an exported function, and returns an object array.
        Object[] results = instance.getFunction("sum").apply(5, 37);
        
        Assert.assertEquals(42, results[0]);
        
        // Drops an instance object pointer which is stored in Rust.
        instance.close();
    }
}
