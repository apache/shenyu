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

package org.apache.shenyu.plugin.wasm.base;

import com.dylibso.chicory.runtime.ByteArrayMemory;
import com.dylibso.chicory.runtime.Memory;
import com.dylibso.chicory.wasm.types.MemoryLimits;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Buffer, result-schema and invocation-lifetime contract tests.
 */
final class WasmRequestBridgeTest {

    private static final byte[] CONTINUE = "{\"version\":1,\"action\":\"CONTINUE\"}".getBytes(StandardCharsets.UTF_8);

    private WasmRequestBridge bridge;

    private Memory memory;

    @BeforeEach
    void setUp() {
        bridge = new WasmRequestBridge();
        memory = new ByteArrayMemory(new MemoryLimits(32));
        begin(1);
    }

    @Test
    void sizeProbeAndUndersizedBufferDoNotWritePartialJson() {
        memory.write(4096, new byte[]{1, 2, 3, 4});
        long required = -bridge.getArgs(memory, 1, 0, 0);
        assertTrue(required > 4);
        assertEquals(-required, bridge.getArgs(memory, 1, 4096, 4));
        assertArrayEquals(new byte[]{1, 2, 3, 4}, memory.readBytes(4096, 4));
        assertEquals(required, bridge.getArgs(memory, 1, 4096, (int) required));
        String json = new String(memory.readBytes(4096, (int) required), StandardCharsets.UTF_8);
        assertTrue(json.contains("\"version\":1"));
        assertTrue(json.contains("你好"));
        assertTrue(json.endsWith("}"));
    }

    @ParameterizedTest
    @CsvSource({"-1,1", "4294967296,1", "2147483647,1", "2097152,1", "0,-1", "0,1048577", "2097151,2"})
    void rejectsInvalidRangesWithoutNarrowingAddresses(final long address, final int length) {
        assertThrows(ShenyuWasmException.class, () -> bridge.getArgs(memory, 1, address, length));
        assertThrows(ShenyuWasmException.class, () -> bridge.putResult(memory, 1, address, length));
    }

    @Test
    void unknownAndExpiredIdsCannotReadOrWriteOtherInvocations() {
        assertThrows(ShenyuWasmException.class, () -> bridge.getArgs(memory, 2, 0, 0));
        assertThrows(ShenyuWasmException.class, () -> bridge.putResult(memory, 2, 0, 1));
        bridge.end();
        assertThrows(ShenyuWasmException.class, () -> bridge.getArgs(memory, 1, 0, 0));
        assertThrows(ShenyuWasmException.class, () -> bridge.putResult(memory, 1, 0, 1));
        begin(2);
        assertThrows(ShenyuWasmException.class, () -> bridge.getArgs(memory, 1, 0, 0));
        assertTrue(bridge.getArgs(memory, 2, 0, 0) < 0);
    }

    @Test
    void resultIsCopiedFromMemoryAndMayOnlyBeSubmittedOnce() {
        memory.write(4096, CONTINUE);
        assertEquals(0, bridge.putResult(memory, 1, 4096, CONTINUE.length));
        memory.fill((byte) 0, 4096, 4096 + CONTINUE.length);
        assertThrows(ShenyuWasmException.class, () -> bridge.putResult(memory, 1, 4096, CONTINUE.length));
        bridge.end();
        begin(2);
        memory.write(4096, CONTINUE);
        assertEquals(0, bridge.putResult(memory, 2, 4096, CONTINUE.length));
    }

    @Test
    void emptyAndMalformedUtf8ResultsAreRejected() {
        assertThrows(ShenyuWasmException.class, () -> bridge.putResult(memory, 1, 0, 0));
        memory.write(4096, new byte[]{(byte) 0xc3, (byte) 0x28});
        assertThrows(ShenyuWasmException.class, () -> bridge.putResult(memory, 1, 4096, 2));
    }

    @Test
    void recursiveInvocationAndClosedBridgeAreRejected() {
        assertThrows(ShenyuWasmException.class, () -> begin(2));
        assertTrue(bridge.getArgs(memory, 1, 0, 0) < 0);
        bridge.close();
        bridge.close();
        assertThrows(ShenyuWasmException.class, () -> begin(2));
        assertThrows(ShenyuWasmException.class, () -> bridge.getArgs(memory, 1, 0, 0));
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "", " ", "null", "[]", "true", "{unquoted:1}",
        "{\"version\":1,\"action\":\"CONTINUE\"} {}",
        "{\"version\":1,\"version\":1,\"action\":\"CONTINUE\"}",
        "{\"version\":4294967297,\"action\":\"CONTINUE\"}",
        "{\"version\":1.0,\"action\":\"CONTINUE\"}",
        "{\"version\":1,\"action\":\"UNKNOWN\"}",
        "{\"version\":1,\"action\":\"CONTINUE\",\"body\":\"\"}",
        "{\"version\":1,\"action\":\"CONTINUE\",\"unsupported\":true}",
        "{\"version\":1,\"action\":\"REJECT\"}",
        "{\"version\":1,\"action\":\"REJECT\",\"status\":200}",
        "{\"version\":1,\"action\":\"REJECT\",\"status\":600}",
        "{\"version\":1,\"action\":\"REJECT\",\"status\":4294967696}",
        "{\"version\":1,\"action\":\"REJECT\",\"status\":403,\"body\":42}",
        "{\"version\":1,\"action\":\"REJECT\",\"status\":403,\"requestHeaders\":{}}",
        "{\"version\":1,\"action\":\"CONTINUE\",\"requestHeaders\":null}",
        "{\"version\":1,\"action\":\"CONTINUE\",\"requestHeaders\":{\"X-Test\":\"text\"}}",
        "{\"version\":1,\"action\":\"CONTINUE\",\"requestHeaders\":{\"X-Test\":[1]}}",
        "{\"version\":1,\"action\":\"CONTINUE\",\"requestHeaders\":{\"X-Test\":[\"ok\"],\"x-test\":[\"duplicate\"]}}",
        "{\"version\":1,\"action\":\"CONTINUE\",\"responseHeaders\":{\"Content-Length\":[\"1\"]}}",
        "{\"version\":1,\"action\":\"CONTINUE\",\"requestHeaders\":{\"X-Test\":[\"bad\\r\\nvalue\"]}}",
        "{\"version\":1,\"action\":\"CONTINUE\",\"requestHeaders\":{\"bad name\":[\"value\"]}}"
    })
    void rejectsInvalidResultSchema(final String json) {
        assertThrows(ShenyuWasmException.class, () -> WasmResult.parse(json.getBytes(StandardCharsets.UTF_8)));
    }

    private void begin(final long id) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/hello").header("X-Test", "你好").build());
        bridge.begin(id, "doExecute", exchange, null, null, null);
    }
}
