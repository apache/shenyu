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

import com.dylibso.chicory.runtime.HostFunction;
import com.dylibso.chicory.runtime.Memory;
import com.dylibso.chicory.runtime.Store;
import com.dylibso.chicory.wasm.types.FunctionType;
import com.dylibso.chicory.wasm.types.ValType;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmException;
import org.springframework.web.server.ServerWebExchange;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Invocation-scoped UTF-8 JSON bridge. The owning plugin serializes access to guest memory.
 */
final class WasmRequestBridge {

    static final int MAX_PAYLOAD_BYTES = 1024 * 1024;

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private long argumentId;

    private byte[] arguments;

    private WasmResult result;

    private boolean closed;

    void register(final Store store) {
        FunctionType signature = FunctionType.of(List.of(ValType.I64, ValType.I64, ValType.I32), List.of(ValType.I32));
        store.addFunction(new HostFunction("shenyu", "get_args", signature,
                (instance, args) -> new long[]{getArgs(instance.memory(), args[0], args[1], (int) args[2])}));
        store.addFunction(new HostFunction("shenyu", "put_result", signature,
                (instance, args) -> new long[]{putResult(instance.memory(), args[0], args[1], (int) args[2])}));
    }

    void begin(final long id, final String phase, final ServerWebExchange exchange,
               final PluginData plugin, final SelectorData selector, final RuleData rule) {
        if (closed || Objects.nonNull(arguments)) {
            throw new ShenyuWasmException("WASM bridge is closed or already executing");
        }
        Map<String, Object> request = new LinkedHashMap<>();
        request.put("method", exchange.getRequest().getMethod().name());
        request.put("uri", exchange.getRequest().getURI().toASCIIString());
        request.put("path", exchange.getRequest().getURI().getRawPath());
        request.put("headers", exchange.getRequest().getHeaders());
        request.put("queryParams", exchange.getRequest().getQueryParams());
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("version", 1);
        payload.put("phase", phase);
        payload.put("request", request);
        payload.put("plugin", plugin);
        payload.put("selector", selector);
        payload.put("rule", rule);
        final byte[] serialized;
        try {
            serialized = MAPPER.writeValueAsBytes(payload);
        } catch (IOException e) {
            throw new ShenyuWasmException("Cannot serialize WASM request", e);
        }
        if (serialized.length > MAX_PAYLOAD_BYTES) {
            throw new ShenyuWasmException("WASM arguments exceed the payload limit");
        }
        argumentId = id;
        arguments = serialized;
        result = null;
    }

    long getArgs(final Memory memory, final long id, final long address, final int capacity) {
        requireInvocation(id);
        checkRange(memory, address, capacity);
        if (capacity < arguments.length) {
            return -arguments.length;
        }
        memory.write((int) address, arguments);
        return arguments.length;
    }

    long putResult(final Memory memory, final long id, final long address, final int length) {
        requireInvocation(id);
        if (Objects.nonNull(result)) {
            throw new ShenyuWasmException("WASM result may only be submitted once");
        }
        checkRange(memory, address, length);
        if (length == 0) {
            throw new ShenyuWasmException("WASM result must be a JSON object");
        }
        result = WasmResult.parse(memory.readBytes((int) address, length));
        return 0;
    }

    private void requireInvocation(final long id) {
        if (Objects.isNull(arguments) || argumentId != id) {
            throw new ShenyuWasmException("Unknown or expired WASM argument ID");
        }
    }

    private void checkRange(final Memory memory, final long address, final int length) {
        if (address < 0 || address > Integer.MAX_VALUE || length < 0 || length > MAX_PAYLOAD_BYTES
                || address + length > (long) memory.pages() * Memory.PAGE_SIZE || address + length > Integer.MAX_VALUE) {
            throw new ShenyuWasmException("Invalid WASM buffer range or payload size");
        }
    }

    long argumentId() {
        return argumentId;
    }

    WasmResult result() {
        return Objects.isNull(result) ? WasmResult.continueChain() : result;
    }

    void end() {
        arguments = null;
        result = null;
        argumentId = 0;
    }

    void close() {
        end();
        closed = true;
    }
}
