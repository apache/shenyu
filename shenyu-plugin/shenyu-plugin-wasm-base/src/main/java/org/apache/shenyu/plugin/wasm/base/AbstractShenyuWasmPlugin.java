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

import com.dylibso.chicory.runtime.ExportFunction;
import com.dylibso.chicory.runtime.Memory;
import com.dylibso.chicory.runtime.Store;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.wasm.api.loader.WasmLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Complex plugins implemented in other languages should extend this class, we still need to write Java subclasses,
 * so we can reuse the convenient/powerful control of ShenYu, such as {@link #getOrder}/{@link #skip}
 * /{@link #skipExcept}/{@link #skipExceptHttpLike}.
 *
 * <p>The no-argument constructor supplies a versioned UTF-8 JSON bridge for request metadata,
 * cached plugin configuration and matched selector/rule DTOs. Guest results can continue the chain,
 * replace headers or reject the request. See the module README for the buffer and result protocol.
 * The Consumer constructor retains custom host adapters.
 *
 * @see org.apache.shenyu.plugin.base.AbstractShenyuPlugin
 * @see org.apache.shenyu.plugin.wasm.api.loader.WasmLoader
 */
public abstract class AbstractShenyuWasmPlugin extends AbstractShenyuPlugin implements AutoCloseable {
    
    protected static final Logger LOG = LoggerFactory.getLogger(AbstractShenyuWasmPlugin.class);
    
    protected static final Map<Long, Argument> ARGUMENTS = new ConcurrentHashMap<>();
    
    protected static final String DO_EXECUTE_METHOD_NAME = "doExecute";
    
    protected static final String BEFORE_METHOD_NAME = "before";
    
    protected static final String AFTER_METHOD_NAME = "after";

    private static final AtomicLong ARGUMENT_IDS = new AtomicLong();

    private final WasmRequestBridge bridge;
    
    private final WasmLoader wasmLoader;
    
    /**
     * Construct a plugin using the supported request/configuration/result bridge.
     */
    public AbstractShenyuWasmPlugin() {
        this.bridge = new WasmRequestBridge();
        this.wasmLoader = new WasmLoader(this.getClass(), bridge::register);
    }

    /**
     * Constructor accepting a custom initializer for advanced use cases where
     * the default built-in host functions ({@code get_args}/{@code put_result})
     * are not sufficient. When using this constructor, the initializer is
     * responsible for registering all required host functions.
     *
     * @param initializer consumer to register host functions on the Chicory Store
     */
    public AbstractShenyuWasmPlugin(final java.util.function.Consumer<Store> initializer) {
        this.bridge = null;
        this.wasmLoader = new WasmLoader(this.getClass(), initializer);
    }
    
    /**
     * Get the WASM module's linear memory for use in host functions.
     *
     * @return the Memory instance
     */
    public Memory getMemory() {
        return wasmLoader.getMemory();
    }
    
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange,
                                   final ShenyuPluginChain chain,
                                   final SelectorData selector,
                                   final RuleData rule) {
        if (Objects.isNull(bridge)) {
            return executeWasm(exchange, chain, selector, rule);
        }
        return Mono.defer(() -> executeWasm(exchange, chain, selector, rule));
    }

    /**
     * Java continuation hook. The supported bridge calls this after applying a CONTINUE result;
     * custom adapters call it after their guest invocation. The default continues the chain.
     *
     * @param exchange   exchange the current server exchange {@linkplain ServerWebExchange}
     * @param chain      chain the current chain  {@linkplain ServerWebExchange}
     * @param selector   selector    {@linkplain SelectorData}
     * @param rule       rule    {@linkplain RuleData}
     * @param argumentId the completed invocation ID; guest callbacks for this ID have expired
     * @return {@code Mono<Void>} to indicate when request handling is complete
     */
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule, final Long argumentId) {
        return chain.execute(exchange);
    }

    private Mono<Void> executeWasm(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                  final SelectorData selector, final RuleData rule) {
        return wasmLoader.getWasmExtern(DO_EXECUTE_METHOD_NAME).map(doExecute -> {
            if (Objects.nonNull(bridge)) {
                return executeBridge(exchange, chain, selector, rule, doExecute);
            }
            final Long argumentId = callWASI(exchange, chain, selector, rule, doExecute);
            return doExecute(exchange, chain, selector, rule, argumentId);
        }).orElseGet(() -> {
            LOG.error("{} function not found in {}", DO_EXECUTE_METHOD_NAME, wasmLoader.getWasmName());
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.WASM_FUNC_NOT_FOUND);
            return WebFluxResultUtils.result(exchange, error);
        });
    }

    private Mono<Void> executeBridge(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                     final SelectorData selector, final RuleData rule, final ExportFunction execute) {
        final Long argumentId = ARGUMENT_IDS.incrementAndGet();
        final WasmResult result;
        try {
            synchronized (wasmLoader) {
                bridge.begin(argumentId, DO_EXECUTE_METHOD_NAME, exchange,
                        BaseDataCache.getInstance().obtainPluginData(named()), selector, rule);
                try {
                    execute.apply(argumentId);
                    result = bridge.result();
                } finally {
                    bridge.end();
                }
            }
        } catch (RuntimeException e) {
            LOG.error("WASM request execution failed in {}", wasmLoader.getWasmName(), e);
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            return exchange.getResponse().setComplete();
        }
        return result.apply(exchange, updated -> doExecute(updated, chain, selector, rule, argumentId));
    }
    
    private Long callWASI(final ServerWebExchange exchange,
                          final ShenyuPluginChain chain,
                          final SelectorData selector,
                          final RuleData rule,
                          final ExportFunction doExecute) {
        // WASI cannot easily pass Java objects like JNI, here we pass Long as arg
        // then we can get the argument by Long
        final Long argumentId = getArgumentId(exchange, chain, selector, rule);
        ARGUMENTS.put(argumentId, new Argument(exchange, chain, selector, rule));
        // call WASI function
        try {
            doExecute.apply(argumentId);
        } finally {
            ARGUMENTS.remove(argumentId);
        }
        return argumentId;
    }
    
    /**
     * Allocate an ID for custom host adapters. The supported bridge allocates its own unique IDs.
     *
     * @param exchange current exchange
     * @param chain plugin chain
     * @param selector matched selector
     * @param rule matched rule
     * @return invocation ID
     */
    protected Long getArgumentId(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                  final SelectorData selector, final RuleData rule) {
        return ARGUMENT_IDS.incrementAndGet();
    }
    
    @Override
    public void before(final ServerWebExchange exchange) {
        wasmLoader.getWasmExtern(BEFORE_METHOD_NAME)
                .ifPresent(before -> callHook(exchange, BEFORE_METHOD_NAME, before));
    }
    
    @Override
    public void after(final ServerWebExchange exchange) {
        wasmLoader.getWasmExtern(AFTER_METHOD_NAME)
                .ifPresent(after -> callHook(exchange, AFTER_METHOD_NAME, after));
    }

    private void callHook(final ServerWebExchange exchange, final String phase, final ExportFunction function) {
        if (Objects.isNull(bridge)) {
            callWASI(exchange, null, null, null, function);
            return;
        }
        synchronized (wasmLoader) {
            bridge.begin(ARGUMENT_IDS.incrementAndGet(), phase, exchange,
                    BaseDataCache.getInstance().obtainPluginData(named()), null, null);
            try {
                function.apply(bridge.argumentId());
            } finally {
                bridge.end();
            }
        }
    }

    @Override
    public void close() {
        synchronized (wasmLoader) {
            if (Objects.nonNull(bridge)) {
                bridge.close();
            }
            wasmLoader.close();
        }
    }
    
    protected static final class Argument {
        
        private final ServerWebExchange exchange;
        
        private final ShenyuPluginChain chain;
        
        private final SelectorData selector;
        
        private final RuleData rule;
        
        private Argument(final ServerWebExchange exchange,
                         final ShenyuPluginChain chain,
                         final SelectorData selector,
                         final RuleData rule) {
            this.exchange = exchange;
            this.chain = chain;
            this.selector = selector;
            this.rule = rule;
        }
        
        public ServerWebExchange getExchange() {
            return exchange;
        }
        
        public ShenyuPluginChain getChain() {
            return chain;
        }
        
        public SelectorData getSelector() {
            return selector;
        }
        
        public RuleData getRule() {
            return rule;
        }
    }
}
