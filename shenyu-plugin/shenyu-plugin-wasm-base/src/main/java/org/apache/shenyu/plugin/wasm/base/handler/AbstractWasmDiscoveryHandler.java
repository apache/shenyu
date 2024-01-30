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

package org.apache.shenyu.plugin.wasm.base.handler;

import io.github.kawamuray.wasmtime.WasmFunctions;
import io.github.kawamuray.wasmtime.WasmValType;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.plugin.base.handler.DiscoveryUpstreamDataHandler;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmInitException;
import org.apache.shenyu.plugin.wasm.api.loader.WasmLoader;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * upstreamList data change.
 */
public abstract class AbstractWasmDiscoveryHandler extends WasmLoader implements DiscoveryUpstreamDataHandler {
    
    protected static final Map<Long, DiscoverySyncData> ARGUMENTS = new ConcurrentHashMap<>();
    
    protected static final String METHOD_NAME = "handlerDiscoveryUpstreamData";
    
    @Override
    public void handlerDiscoveryUpstreamData(final DiscoverySyncData discoverySyncData) {
        super.getWasmExtern(METHOD_NAME)
                .map(handlerDiscoveryUpstreamData -> {
                    // WASI cannot easily pass Java objects like JNI, here we pass Long as arg
                    // then we can get the argument by Long
                    final Long argumentId = getArgumentId(discoverySyncData);
                    ARGUMENTS.put(argumentId, discoverySyncData);
                    // call WASI function
                    WasmFunctions.consumer(super.getStore(), handlerDiscoveryUpstreamData.func(), WasmValType.I64)
                            .accept(argumentId);
                    ARGUMENTS.remove(argumentId);
                    return argumentId;
                }).orElseThrow(() -> new ShenyuWasmInitException(METHOD_NAME + " function not find in wasm file: " + getWasmName()));
    }
    
    protected abstract Long getArgumentId(DiscoverySyncData discoverySyncData);
}
