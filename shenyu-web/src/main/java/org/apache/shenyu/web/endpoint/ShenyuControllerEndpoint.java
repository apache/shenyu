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

package org.apache.shenyu.web.endpoint;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.web.diagnostics.DiagnosticRedactor;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Flux;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The type Shenyu controller endpoint.
 *
 * <p>These mappings bypass the gateway plugin chain (the {@code HealthFilter} dispatches
 * {@code /actuator/*} straight to the {@code DispatcherHandler}), so the raw configuration
 * caches must never be serialized here: plugin configs, selector and rule handles and RPC
 * metadata routinely carry credentials. The whole controller is therefore
 * <b>disabled by default</b> ({@code shenyu.endpoint.raw-data.enabled}, default {@code false});
 * a request that finds no mapping is rejected before any cache traversal. Even when explicitly
 * enabled, the endpoints only publish a bounded, non-reversible representation: SHA-256
 * digests of the sensitive payloads plus non-secret structural counters, reusing the shared
 * {@link DiagnosticRedactor#digest(String)} policy from the diagnostics subsystem. Full raw
 * inspection belongs to the management plane (shenyu-admin), not to a listener that anonymous
 * callers can reach.</p>
 *
 * <p>Note these are plain MVC mappings under a hard-coded {@code /actuator} prefix; they are
 * not Spring Boot Actuator endpoints, so {@code management.endpoints.web.exposure.include}
 * and {@code management.server} base-path/port settings have no effect on them. Use the
 * {@code shenyu.endpoint.raw-data.enabled} switch instead.</p>
 */
@RestController
@ConditionalOnProperty(name = "shenyu.endpoint.raw-data.enabled", havingValue = "true", matchIfMissing = false)
@RequestMapping(value = "/actuator", produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
public class ShenyuControllerEndpoint {

    private final ShenyuWebHandler webHandler;

    private final DiagnosticRedactor redactor = new DiagnosticRedactor();

    /**
     * Instantiates a new Shenyu controller endpoint.
     *
     * @param webHandler the web handler
     */
    public ShenyuControllerEndpoint(final ShenyuWebHandler webHandler) {
        this.webHandler = webHandler;
    }

    /**
     * Plugins flux.
     *
     * @return the flux
     */
    @GetMapping("/plugins")
    public Flux<Map<String, Integer>> plugins() {
        List<ShenyuPlugin> plugins = webHandler.getPlugins();
        return Flux.just(plugins.stream().collect(Collectors.toMap(ShenyuPlugin::toString, ShenyuPlugin::getOrder)));
    }

    /**
     * Plugin data digests flux.
     *
     * @return the flux
     */
    @GetMapping("/pluginData")
    public Flux<Map<String, Map<String, Object>>> pluginDatas() {
        return Flux.just(BaseDataCache.getInstance().getPluginMap().entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, entry -> summarizePlugin(entry.getValue()), (a, b) -> a, LinkedHashMap::new)));
    }

    /**
     * Selector data digests flux.
     *
     * @return the flux
     */
    @GetMapping("/selectorData")
    public Flux<Map<String, List<Map<String, Object>>>> selectorData() {
        return Flux.just(summarizeSelectors(BaseDataCache.getInstance().getSelectorMap()));
    }

    /**
     * Rule data digests flux.
     *
     * @return the flux
     */
    @GetMapping("/ruleData")
    public Flux<Map<String, List<Map<String, Object>>>> ruleData() {
        return Flux.just(summarizeRules(BaseDataCache.getInstance().getRuleMap()));
    }

    /**
     * selector match cache digests flux.
     *
     * @return the flux
     */
    @GetMapping("/selectorMatchCache")
    public Flux<Map<String, List<Map<String, Object>>>> getSelectorMatchCache() {
        return Flux.just(summarizeSelectorMatch(MatchDataCache.getInstance().getSelectorMatchCache()));
    }

    /**
     * rule match cache digests flux.
     *
     * @return the flux
     */
    @GetMapping("/ruleMatchCache")
    public Flux<Map<String, List<Map<String, Object>>>> getRuleMatchCache() {
        return Flux.just(summarizeRuleMatch(MatchDataCache.getInstance().getRuleMatchCache()));
    }

    /**
     * get metadata digests map.
     *
     * @return metadata digest map
     */
    @GetMapping("/metadata")
    public Flux<Map<String, Map<String, Object>>> getMetadata() {
        return Flux.just(summarizeMetadata(MetaDataCache.getInstance().getMetaDataMap()));
    }

    /**
     * get metadata cache digests.
     *
     * @return metadata digest map
     */
    @GetMapping("/metadataCache")
    public Flux<Map<String, Map<String, Object>>> getMetaDataCache() {
        return Flux.just(summarizeMetadata(MetaDataCache.getInstance().getMetaDataCache()));
    }

    private Map<String, Object> summarizePlugin(final PluginData data) {
        Map<String, Object> item = new LinkedHashMap<>();
        item.put("role", data.getRole());
        item.put("configDigest", redactor.digest(data.getConfig()));
        return item;
    }

    private Map<String, List<Map<String, Object>>> summarizeSelectors(final Map<String, List<SelectorData>> selectors) {
        Map<String, List<Map<String, Object>>> root = new LinkedHashMap<>();
        for (Map.Entry<String, List<SelectorData>> entry : selectors.entrySet()) {
            List<Map<String, Object>> list = new ArrayList<>(entry.getValue().size());
            for (SelectorData data : entry.getValue()) {
                Map<String, Object> item = new LinkedHashMap<>();
                item.put("selectorId", data.getId());
                item.put("handleDigest", redactor.digest(data.getHandle()));
                item.put("conditionCount", size(data.getConditionList()));
                list.add(item);
            }
            root.put(entry.getKey(), list);
        }
        return root;
    }

    private Map<String, List<Map<String, Object>>> summarizeRules(final Map<String, List<RuleData>> rules) {
        Map<String, List<Map<String, Object>>> root = new LinkedHashMap<>();
        for (Map.Entry<String, List<RuleData>> entry : rules.entrySet()) {
            List<Map<String, Object>> list = new ArrayList<>(entry.getValue().size());
            for (RuleData data : entry.getValue()) {
                Map<String, Object> item = new LinkedHashMap<>();
                item.put("ruleId", data.getId());
                item.put("pluginName", data.getPluginName());
                item.put("handleDigest", redactor.digest(data.getHandle()));
                item.put("conditionCount", size(data.getConditionDataList()));
                list.add(item);
            }
            root.put(entry.getKey(), list);
        }
        return root;
    }

    private Map<String, List<Map<String, Object>>> summarizeSelectorMatch(final Map<String, Map<String, SelectorData>> selectors) {
        Map<String, List<Map<String, Object>>> root = new LinkedHashMap<>();
        for (Map.Entry<String, Map<String, SelectorData>> entry : selectors.entrySet()) {
            List<Map<String, Object>> list = new ArrayList<>(entry.getValue().size());
            for (SelectorData data : entry.getValue().values()) {
                Map<String, Object> item = new LinkedHashMap<>();
                item.put("selectorId", data.getId());
                item.put("handleDigest", redactor.digest(data.getHandle()));
                item.put("conditionCount", size(data.getConditionList()));
                list.add(item);
            }
            root.put(entry.getKey(), list);
        }
        return root;
    }

    private Map<String, List<Map<String, Object>>> summarizeRuleMatch(final Map<String, Map<String, RuleData>> rules) {
        Map<String, List<Map<String, Object>>> root = new LinkedHashMap<>();
        for (Map.Entry<String, Map<String, RuleData>> entry : rules.entrySet()) {
            List<Map<String, Object>> list = new ArrayList<>(entry.getValue().size());
            for (RuleData data : entry.getValue().values()) {
                Map<String, Object> item = new LinkedHashMap<>();
                item.put("ruleId", data.getId());
                item.put("pluginName", data.getPluginName());
                item.put("handleDigest", redactor.digest(data.getHandle()));
                item.put("conditionCount", size(data.getConditionDataList()));
                list.add(item);
            }
            root.put(entry.getKey(), list);
        }
        return root;
    }

    private Map<String, Map<String, Object>> summarizeMetadata(final Map<String, MetaData> metadata) {
        Map<String, Map<String, Object>> root = new LinkedHashMap<>();
        for (Map.Entry<String, MetaData> entry : metadata.entrySet()) {
            MetaData data = entry.getValue();
            Map<String, Object> item = new LinkedHashMap<>();
            item.put("rpcType", data.getRpcType());
            item.put("pathDigest", redactor.digest(data.getPath()));
            item.put("rpcExtDigest", redactor.digest(data.getRpcExt()));
            root.put(entry.getKey(), item);
        }
        return root;
    }

    private int size(final List<?> list) {
        return Objects.isNull(list) ? 0 : list.size();
    }

}
