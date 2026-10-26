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

package org.apache.shenyu.plugin.wasm.base.gateway;

import com.dylibso.chicory.runtime.HostFunction;
import com.dylibso.chicory.runtime.Instance;
import com.dylibso.chicory.runtime.Store;
import com.dylibso.chicory.wasm.types.FunctionType;
import com.dylibso.chicory.wasm.types.ValType;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmException;
import org.apache.shenyu.plugin.wasm.base.AbstractShenyuWasmPlugin;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Reference token gate using the binary request profile documented in ABI-v1.md.
 */
public class ReferenceWasmPlugin extends AbstractShenyuWasmPlugin {

    static final String NAME = "wasmReference";

    static final String RESULT = ReferenceWasmPlugin.class.getName() + ".result";

    private static final String BODY = ReferenceWasmPlugin.class.getName() + ".body";

    private static final List<String> FIELDS = List.of("selectorId", "selectorHandle", "ruleId", "ruleHandle",
            "method", "path", "query", "token", "requestId", "body");

    private static final AtomicLong IDS = new AtomicLong();

    public ReferenceWasmPlugin() {
        super(ReferenceWasmPlugin::registerImports);
    }

    private static void registerImports(final Store store) {
        FunctionType type = FunctionType.of(List.of(ValType.I64, ValType.I64, ValType.I32), List.of(ValType.I32));
        store.addFunction(new HostFunction("shenyu", "get_args", type, ReferenceWasmPlugin::getArgs));
        store.addFunction(new HostFunction("shenyu", "put_result", type, ReferenceWasmPlugin::putResult));
    }

    private static long[] getArgs(final Instance instance, final long... args) {
        Argument argument = ARGUMENTS.get(args[0]);
        if (Objects.isNull(argument)) {
            return new long[]{-2};
        }
        ServerWebExchange exchange = argument.getExchange();
        List<String> fields = List.of(argument.getSelector().getId(), argument.getSelector().getHandle(),
                argument.getRule().getId(), argument.getRule().getHandle(), exchange.getRequest().getMethod().name(),
                exchange.getRequest().getURI().getRawPath(), Objects.toString(exchange.getRequest().getURI().getRawQuery(), ""),
                Objects.toString(exchange.getRequest().getHeaders().getFirst("X-Token"), ""),
                Objects.toString(exchange.getRequest().getHeaders().getFirst("X-Request-Id"), ""), exchange.getAttributeOrDefault(BODY, ""));
        List<byte[]> bytes = fields.stream().map(field -> field.getBytes(StandardCharsets.UTF_8)).toList();
        int size = Integer.BYTES + bytes.stream().mapToInt(field -> Integer.BYTES + field.length).sum();
        if (args[2] < size) {
            // No partial write: the guest retries with its full buffer.
            return new long[]{-1};
        }
        ByteBuffer frame = ByteBuffer.allocate(size).order(ByteOrder.LITTLE_ENDIAN);
        frame.putInt(1);
        bytes.forEach(field -> frame.putInt(field.length).put(field));
        instance.memory().write(Math.toIntExact(args[1]), frame.array());
        return new long[]{size};
    }

    private static long[] putResult(final Instance instance, final long... args) {
        Argument argument = ARGUMENTS.get(args[0]);
        if (Objects.isNull(argument)) {
            return new long[]{-2};
        }
        byte[] bytes = instance.memory().readBytes(Math.toIntExact(args[1]), Math.toIntExact(args[2]));
        ByteBuffer frame = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN);
        int decision = frame.getInt();
        if (frame.getInt() != 1 || decision < 0 || decision > 1) {
            throw new ShenyuWasmException("Invalid reference result version or decision");
        }
        Map<String, String> result = new LinkedHashMap<>();
        result.put("decision", decision == 0 ? "allow" : "deny");
        for (String field : FIELDS) {
            int length = frame.getInt();
            if (length < 0 || length > frame.remaining()) {
                throw new ShenyuWasmException("Invalid reference result field length");
            }
            byte[] value = new byte[length];
            frame.get(value);
            result.put(field, new String(value, StandardCharsets.UTF_8));
        }
        if (frame.hasRemaining()) {
            throw new ShenyuWasmException("Unexpected reference result bytes");
        }
        argument.getExchange().getAttributes().put(RESULT, result);
        return new long[]{0};
    }

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        // Buffer only in this example. A production adapter must define body size and streaming policy.
        return DataBufferUtils.join(exchange.getRequest().getBody(), 32768).map(buffer -> {
            try {
                byte[] body = new byte[buffer.readableByteCount()];
                buffer.read(body);
                return new String(body, StandardCharsets.UTF_8);
            } finally {
                DataBufferUtils.release(buffer);
            }
        }).defaultIfEmpty("").flatMap(body -> {
            exchange.getAttributes().put(BODY, body);
            // The loader owns one mutable instance. Hold the monitor for the synchronous
            // guest invocation and result decoding, then run the reactive chain outside it.
            synchronized (this) {
                return super.execute(exchange, chain);
            }
        });
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule, final Long argumentId) {
        Map<String, String> result = exchange.getAttribute(RESULT);
        if (Objects.isNull(result)) {
            return Mono.error(new ShenyuWasmException("Guest did not return a decision"));
        }
        exchange.getResponse().getHeaders().set("X-Wasm-Decision", result.get("decision"));
        if ("deny".equals(result.get("decision"))) {
            exchange.getResponse().setStatusCode(HttpStatus.FORBIDDEN);
            return WebFluxResultUtils.result(exchange, result);
        }
        return chain.execute(exchange);
    }

    @Override
    protected Long getArgumentId(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                 final SelectorData selector, final RuleData rule) {
        return IDS.incrementAndGet();
    }

    @Override
    public int getOrder() {
        return -100;
    }

    @Override
    public String named() {
        return NAME;
    }

    static int pendingArguments() {
        return ARGUMENTS.size();
    }
}
