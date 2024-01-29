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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.wasm.api.loader.WasmLoader;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * AbstractWasmPluginDataHandler.
 */
public abstract class AbstractWasmPluginDataHandler extends WasmLoader implements PluginDataHandler {
    
    protected static final Map<Long, PluginData> PLUGIN_ARGUMENTS = new ConcurrentHashMap<>();
    
    protected static final String HANDLER_PLUGIN_METHOD_NAME = "handlerPlugin";
    
    protected static final String REMOVE_PLUGIN_METHOD_NAME = "removePlugin";
    
    protected static final Map<Long, SelectorData> SELECTOR_ARGUMENTS = new ConcurrentHashMap<>();
    
    protected static final String HANDLER_SELECTOR_METHOD_NAME = "handlerSelector";
    
    protected static final String REMOVE_SELECTOR_METHOD_NAME = "removeSelector";
    
    protected static final Map<Long, RuleData> RULE_ARGUMENTS = new ConcurrentHashMap<>();
    
    protected static final String HANDLER_RULE_METHOD_NAME = "handlerRule";
    
    protected static final String REMOVE_RULE_METHOD_NAME = "removeRule";
    
    @Override
    public void handlerPlugin(final PluginData pluginData) {
        super.getWasmExtern(HANDLER_PLUGIN_METHOD_NAME)
                .ifPresent(handlerPlugin -> callWASI(pluginData, handlerPlugin));
    }
    
    @Override
    public void removePlugin(final PluginData pluginData) {
        super.getWasmExtern(REMOVE_PLUGIN_METHOD_NAME)
                .ifPresent(handlerPlugin -> callWASI(pluginData, handlerPlugin));
    }
    
    @Override
    public void handlerSelector(final SelectorData selectorData) {
        super.getWasmExtern(HANDLER_SELECTOR_METHOD_NAME)
                .ifPresent(handlerPlugin -> callWASI(selectorData, handlerPlugin));
    }
    
    @Override
    public void removeSelector(final SelectorData selectorData) {
        super.getWasmExtern(REMOVE_SELECTOR_METHOD_NAME)
                .ifPresent(handlerPlugin -> callWASI(selectorData, handlerPlugin));
    }
    
    @Override
    public void handlerRule(final RuleData ruleData) {
        super.getWasmExtern(HANDLER_RULE_METHOD_NAME)
                .ifPresent(handlerPlugin -> callWASI(ruleData, handlerPlugin));
    }
    
    @Override
    public void removeRule(final RuleData ruleData) {
        super.getWasmExtern(REMOVE_RULE_METHOD_NAME)
                .ifPresent(handlerPlugin -> callWASI(ruleData, handlerPlugin));
    }
    
    private Long callWASI(final PluginData pluginData, final Extern execute) {
        // WASI cannot easily pass Java objects like JNI, here we pass Long as arg
        // then we can get the argument by Long
        final Long argumentId = getPluginArgumentId(pluginData);
        PLUGIN_ARGUMENTS.put(argumentId, pluginData);
        // call WASI function
        WasmFunctions.consumer(super.getStore(), execute.func(), WasmValType.I64)
                .accept(argumentId);
        PLUGIN_ARGUMENTS.remove(argumentId);
        return argumentId;
    }
    
    private Long callWASI(final RuleData ruleData, final Extern execute) {
        // WASI cannot easily pass Java objects like JNI, here we pass Long as arg
        // then we can get the argument by Long
        final Long argumentId = getRuleArgumentId(ruleData);
        RULE_ARGUMENTS.put(argumentId, ruleData);
        // call WASI function
        WasmFunctions.consumer(super.getStore(), execute.func(), WasmValType.I64)
                .accept(argumentId);
        RULE_ARGUMENTS.remove(argumentId);
        return argumentId;
    }
    
    private Long callWASI(final SelectorData selectorData, final Extern execute) {
        // WASI cannot easily pass Java objects like JNI, here we pass Long as arg
        // then we can get the argument by Long
        final Long argumentId = getSelectorArgumentId(selectorData);
        SELECTOR_ARGUMENTS.put(argumentId, selectorData);
        // call WASI function
        WasmFunctions.consumer(super.getStore(), execute.func(), WasmValType.I64)
                .accept(argumentId);
        SELECTOR_ARGUMENTS.remove(argumentId);
        return argumentId;
    }
    
    protected Long getPluginArgumentId(final PluginData pluginData) {
        return 0L;
    }
    
    protected Long getSelectorArgumentId(final SelectorData selectorData) {
        return 0L;
    }
    
    protected Long getRuleArgumentId(final RuleData ruleData) {
        return 0L;
    }
}
