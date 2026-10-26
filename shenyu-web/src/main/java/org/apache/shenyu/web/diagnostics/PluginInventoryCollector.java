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

package org.apache.shenyu.web.diagnostics;

import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.web.handler.ShenyuWebHandler;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * Collects the ordered inventory of plugins actually composed into this gateway.
 *
 * <p>Only the plugin identity and order are published; no plugin configuration or rule handle is
 * included here. Configuration is summarised separately as redacted digests by
 * {@link EffectiveConfigDigestCollector}.</p>
 */
public final class PluginInventoryCollector implements SectionCollector {

    private static final String NAME = "plugins";

    private final ShenyuWebHandler webHandler;

    /**
     * Instantiates a new plugin inventory collector.
     *
     * @param webHandler the gateway web handler exposing the live plugin chain
     */
    public PluginInventoryCollector(final ShenyuWebHandler webHandler) {
        this.webHandler = webHandler;
    }

    @Override
    public String name() {
        return NAME;
    }

    @Override
    public DiagnosticSection collect(final DiagnosticBudget budget) {
        final long observedAt = System.currentTimeMillis();
        final long startNanos = System.nanoTime();
        if (budget.shouldStop()) {
            return DiagnosticSection.of(NAME, SectionStatus.SKIPPED, observedAt, 0L, "budget exhausted before start");
        }
        List<ShenyuPlugin> plugins = webHandler.getPlugins();
        List<Map<String, Object>> inventory = new ArrayList<>(plugins.size());
        boolean complete = true;
        for (ShenyuPlugin plugin : plugins) {
            if (budget.shouldStop()) {
                complete = false;
                break;
            }
            inventory.add(describe(plugin));
        }
        Map<String, Object> root = new LinkedHashMap<>();
        root.put("count", plugins.size());
        root.put("collected", inventory.size());
        root.put("plugins", inventory);
        String json = GsonUtils.getInstance().toJson(root);
        if (!complete) {
            return DiagnosticSection.truncated(NAME, observedAt, elapsedMs(startNanos), json.getBytes(StandardCharsets.UTF_8), "budget exhausted during inventory");
        }
        return DiagnosticSection.ok(NAME, observedAt, elapsedMs(startNanos), json);
    }

    private Map<String, Object> describe(final ShenyuPlugin plugin) {
        Map<String, Object> entry = new LinkedHashMap<>();
        String named = plugin.named();
        entry.put("name", named.isEmpty() ? plugin.getClass().getSimpleName() : named);
        entry.put("class", plugin.getClass().getName());
        entry.put("order", plugin.getOrder());
        return entry;
    }

    private long elapsedMs(final long startNanos) {
        return TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos);
    }
}
