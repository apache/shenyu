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

import io.github.kawamuray.wasmtime.Extern;
import io.github.kawamuray.wasmtime.WasmFunctions;
import io.github.kawamuray.wasmtime.WasmValType;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.wasm.api.loader.WasmLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Simple plugins implemented in other languages should extend this class, we still need to write Java subclasses,
 * so we can reuse the convenient/powerful control of ShenYu, such as {@link #getOrder}/{@link #skip}
 * /{@link #skipExcept}/{@link #skipExceptHttpLike}.
 *
 * @see org.apache.shenyu.plugin.api.ShenyuPlugin
 * @see io.github.kawamuray.wasmtime.WasmValType
 * @see WasmLoader
 */
public abstract class AbstractWasmPlugin extends WasmLoader implements ShenyuPlugin {
    
    protected static final Logger LOG = LoggerFactory.getLogger(AbstractWasmPlugin.class);
    
    protected static final Map<Long, Argument> ARGUMENTS = new ConcurrentHashMap<>();
    
    protected static final String EXECUTE_METHOD_NAME = "execute";
    
    protected static final String BEFORE_METHOD_NAME = "before";
    
    protected static final String AFTER_METHOD_NAME = "after";
    
    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return super.getWasmExtern(EXECUTE_METHOD_NAME).map(execute -> {
            final Long argumentId = callWASI(exchange, chain, execute);
            return doExecute(exchange, chain, argumentId);
        }).orElseGet(() -> {
            LOG.error("{} function not found in {}", EXECUTE_METHOD_NAME, super.getWasmName());
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.WASM_FUNC_NOT_FOUND);
            return WebFluxResultUtils.result(exchange, error);
        });
    }
    
    /**
     * this is Template Method child has implements your own logic.
     *
     * @param exchange   exchange the current server exchange {@linkplain ServerWebExchange}
     * @param chain      chain the current chain {@linkplain ServerWebExchange}
     * @param argumentId the argument id {@linkplain #getArgumentId}
     * @return {@code Mono<Void>} to indicate when request handling is complete
     */
    protected abstract Mono<Void> doExecute(ServerWebExchange exchange, ShenyuPluginChain chain, Long argumentId);
    
    private Long callWASI(final ServerWebExchange exchange, final ShenyuPluginChain chain, final Extern execute) {
        // WASI cannot easily pass Java objects like JNI, here we pass Long as arg
        // then we can get the argument by Long
        final Long argumentId = getArgumentId(exchange, chain);
        ARGUMENTS.put(argumentId, new Argument(exchange, chain));
        // call WASI function
        WasmFunctions.consumer(super.getStore(), execute.func(), WasmValType.I64)
                .accept(argumentId);
        ARGUMENTS.remove(argumentId);
        return argumentId;
    }
    
    protected abstract Long getArgumentId(ServerWebExchange exchange, ShenyuPluginChain chain);
    
    @Override
    public void before(final ServerWebExchange exchange) {
        super.getWasmExtern(BEFORE_METHOD_NAME)
                .ifPresent(before -> callWASI(exchange, null, before));
    }
    
    @Override
    public void after(final ServerWebExchange exchange) {
        super.getWasmExtern(AFTER_METHOD_NAME)
                .ifPresent(before -> callWASI(exchange, null, before));
    }
    
    protected static final class Argument {
        
        private final ServerWebExchange exchange;
        
        private final ShenyuPluginChain chain;
        
        private Argument(final ServerWebExchange exchange,
                         final ShenyuPluginChain chain) {
            this.exchange = exchange;
            this.chain = chain;
        }
        
        public ServerWebExchange getExchange() {
            return exchange;
        }
        
        public ShenyuPluginChain getChain() {
            return chain;
        }
    }
}
