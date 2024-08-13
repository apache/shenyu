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

import io.github.kawamuray.wasmtime.Extern;
import io.github.kawamuray.wasmtime.WasmFunctions;
import io.github.kawamuray.wasmtime.WasmValType;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmInitException;
import org.apache.shenyu.plugin.wasm.api.loader.WasmLoader;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The type abstract wasm metadata subscriber.
 */
public abstract class AbstractWasmMetaDataHandler extends WasmLoader implements MetaDataHandler {
    
    protected static final Map<Long, MetaData> ARGUMENTS = new ConcurrentHashMap<>();
    
    protected static final String HANDLE_METHOD_NAME = "handleMetaData";
    
    protected static final String REMOVE_METHOD_NAME = "removeMetaData";
    
    protected static final String REFRESH_METHOD_NAME = "refresh";
    
    @Override
    public void handle(final MetaData metaData) {
        super.getWasmExtern(HANDLE_METHOD_NAME)
                .map(handle -> callWASI(metaData, handle))
                .orElseThrow(() -> new ShenyuWasmInitException(HANDLE_METHOD_NAME + " function not find in wasm file: " + getWasmName()));
    }
    
    @Override
    public void remove(final MetaData metaData) {
        super.getWasmExtern(REMOVE_METHOD_NAME)
                .map(remove -> callWASI(metaData, remove))
                .orElseThrow(() -> new ShenyuWasmInitException(REMOVE_METHOD_NAME + " function not find in wasm file: " + getWasmName()));
    }
    
    private Long callWASI(final MetaData metaData, final Extern execute) {
        // WASI cannot easily pass Java objects like JNI, here we pass Long as arg
        // then we can get the argument by Long
        final Long argumentId = getArgumentId(metaData);
        ARGUMENTS.put(argumentId, metaData);
        // call WASI function
        WasmFunctions.consumer(super.getStore(), execute.func(), WasmValType.I64)
                .accept(argumentId);
        ARGUMENTS.remove(argumentId);
        return argumentId;
    }
    
    protected abstract Long getArgumentId(MetaData metaData);
    
    @Override
    public void refresh() {
        super.getWasmExtern(REFRESH_METHOD_NAME)
                .ifPresent(refresh -> WasmFunctions.consumer(super.getStore(), refresh.func()).accept());
    }
}
