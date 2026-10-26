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

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;

/**
 * Summarises the effective plugin, selector and rule configuration as redacted digests.
 *
 * <p>Selector and rule handles routinely carry credentials, upstream secrets and tokens. This
 * collector never publishes a raw handle or plugin config: it emits a stable SHA-256 digest so
 * two bundles can be compared for "did the config change" without ever revealing the value, plus
 * non-secret structural counters. The rendered document is passed through the shared redaction
 * policy again as defense in depth.</p>
 */
public final class EffectiveConfigDigestCollector implements SectionCollector {

    private static final String NAME = "config-digest";

    private final DiagnosticRedactor redactor;

    /**
     * Instantiates a new effective config digest collector.
     *
     * @param redactor the shared redaction policy
     */
    public EffectiveConfigDigestCollector(final DiagnosticRedactor redactor) {
        this.redactor = redactor;
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
        BaseDataCache base = BaseDataCache.getInstance();
        Map<String, Object> root = new LinkedHashMap<>();
        root.put("plugins", summarizePlugins(base.getPluginMap(), budget));
        root.put("selectors", summarizeSelectors(base.getSelectorMap(), budget));
        root.put("rules", summarizeRules(base.getRuleMap(), budget));
        root.put("metadataCount", MetaDataCache.getInstance().getMetaDataMap().size());
        String redacted = redactor.redactJson(GsonUtils.getInstance().toJson(root));
        return DiagnosticSection.ok(NAME, observedAt, elapsedMs(startNanos), redacted);
    }

    private List<Map<String, Object>> summarizePlugins(final ConcurrentMap<String, PluginData> plugins, final DiagnosticBudget budget) {
        List<Map<String, Object>> list = new ArrayList<>(plugins.size());
        for (Map.Entry<String, PluginData> entry : plugins.entrySet()) {
            if (budget.shouldStop()) {
                break;
            }
            PluginData data = entry.getValue();
            Map<String, Object> item = new LinkedHashMap<>();
            item.put("name", entry.getKey());
            item.put("role", data.getRole());
            item.put("configDigest", redactor.digest(data.getConfig()));
            list.add(item);
        }
        return list;
    }

    private List<Map<String, Object>> summarizeSelectors(final ConcurrentMap<String, List<SelectorData>> selectors, final DiagnosticBudget budget) {
        List<Map<String, Object>> list = new ArrayList<>();
        for (Map.Entry<String, List<SelectorData>> entry : selectors.entrySet()) {
            for (SelectorData data : entry.getValue()) {
                if (budget.shouldStop()) {
                    return list;
                }
                Map<String, Object> item = new LinkedHashMap<>();
                item.put("pluginName", entry.getKey());
                item.put("handleDigest", redactor.digest(data.getHandle()));
                item.put("conditionCount", size(data.getConditionList()));
                list.add(item);
            }
        }
        return list;
    }

    private List<Map<String, Object>> summarizeRules(final ConcurrentMap<String, List<RuleData>> rules, final DiagnosticBudget budget) {
        List<Map<String, Object>> list = new ArrayList<>();
        for (Map.Entry<String, List<RuleData>> entry : rules.entrySet()) {
            for (RuleData data : entry.getValue()) {
                if (budget.shouldStop()) {
                    return list;
                }
                Map<String, Object> item = new LinkedHashMap<>();
                item.put("selectorId", entry.getKey());
                item.put("pluginName", data.getPluginName());
                item.put("handleDigest", redactor.digest(data.getHandle()));
                item.put("conditionCount", size(data.getConditionDataList()));
                list.add(item);
            }
        }
        return list;
    }

    private int size(final List<?> list) {
        return Objects.isNull(list) ? 0 : list.size();
    }

    private long elapsedMs(final long startNanos) {
        return TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos);
    }
}
