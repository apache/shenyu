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

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.wasm.base.handler.AbstractWasmPluginDataHandler;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Configuration hooks acknowledge the exact JSON received by the guest.
 */
public final class ReferenceWasmPluginDataHandler extends AbstractWasmPluginDataHandler {

    private static final AtomicLong IDS = new AtomicLong();

    private final List<Map<String, Object>> events = new CopyOnWriteArrayList<>();

    @Override
    protected long onGetArgs(final long argId, final long addr, final int len) {
        Object data = PLUGIN_ARGUMENTS.get(argId);
        if (Objects.isNull(data)) {
            data = SELECTOR_ARGUMENTS.get(argId);
        }
        if (Objects.isNull(data)) {
            data = RULE_ARGUMENTS.get(argId);
        }
        if (Objects.isNull(data)) {
            return -2;
        }
        byte[] bytes = GsonUtils.getInstance().toJson(data).getBytes(StandardCharsets.UTF_8);
        if (bytes.length > len) {
            return -1;
        }
        getMemory().write(Math.toIntExact(addr), bytes);
        return bytes.length;
    }

    @Override
    protected long onPutResult(final long argId, final long addr, final int len) {
        byte[] bytes = getMemory().readBytes(Math.toIntExact(addr), len);
        int event = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).getInt();
        String json = new String(bytes, Integer.BYTES, bytes.length - Integer.BYTES, StandardCharsets.UTF_8);
        events.add(Map.of("event", event, "json", json));
        return 0;
    }

    @Override
    protected Long getPluginArgumentId(final PluginData data) {
        return IDS.incrementAndGet();
    }

    @Override
    protected Long getSelectorArgumentId(final SelectorData data) {
        return IDS.incrementAndGet();
    }

    @Override
    protected Long getRuleArgumentId(final RuleData data) {
        return IDS.incrementAndGet();
    }

    @Override
    public synchronized void handlerPlugin(final PluginData data) {
        super.handlerPlugin(data);
    }

    @Override
    public synchronized void removePlugin(final PluginData data) {
        super.removePlugin(data);
    }

    @Override
    public synchronized void handlerSelector(final SelectorData data) {
        super.handlerSelector(data);
    }

    @Override
    public synchronized void removeSelector(final SelectorData data) {
        super.removeSelector(data);
    }

    @Override
    public synchronized void handlerRule(final RuleData data) {
        super.handlerRule(data);
    }

    @Override
    public synchronized void removeRule(final RuleData data) {
        super.removeRule(data);
    }

    @Override
    public String pluginNamed() {
        return ReferenceWasmPlugin.NAME;
    }

    List<Map<String, Object>> events() {
        return events;
    }

    static int pendingArguments() {
        return PLUGIN_ARGUMENTS.size() + SELECTOR_ARGUMENTS.size() + RULE_ARGUMENTS.size();
    }
}
