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

package org.apache.shenyu.plugin.wasm.api;

import org.apache.shenyu.plugin.wasm.api.loader.WasmLoader;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Export lookup must distinguish callable exports from absent or non-function exports.
 */
final class WasmLoaderTest {

    @Test
    void lookupReturnsOnlyExportedFunctions() {
        try (WasmLoader loader = new WasmLoader(AbstractWasmPluginTest.RustWasmPlugin.class, null)) {
            assertTrue(loader.getWasmExtern("execute").isPresent());
            assertFalse(loader.getWasmExtern("does_not_exist").isPresent());
            assertFalse(loader.getWasmExtern("memory").isPresent());
        }
    }
}
