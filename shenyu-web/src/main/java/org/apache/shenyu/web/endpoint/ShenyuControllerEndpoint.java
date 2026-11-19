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
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * The type Shenyu controller endpoint.
 *
 * <p>This controller publishes the <b>always-available digest view</b> of the runtime
 * configuration caches. These mappings bypass the gateway plugin chain (the {@code HealthFilter}
 * dispatches {@code /actuator/*} straight to the {@code DispatcherHandler}), so a secret-bearing
 * payload must never be serialized here: plugin configs, selector and rule handles and RPC
 * metadata routinely carry credentials. The digest endpoints publish non-secret identity fields
 * (plugin, selector and rule names, metadata paths, RPC types) plus SHA-256 digests of the
 * sensitive payload fields only ({@code config}, {@code handle}, {@code rpcExt}), reusing the
 * shared {@link DiagnosticRedactor#digest(String)} policy from the diagnostics subsystem. The
 * raw caches live behind the opt-in {@code shenyu.endpoint.raw-data.enabled} switch served by
 * {@link ShenyuRawDataEndpoint}; see {@code docs/actuator-raw-data.md} for the compatibility
 * and migration behavior.</p>
 *
 * <p>Only the {@code /actuator/plugins} listing (plugin name and order, no configuration at
 * all) is exposed without a counterpart; it stays visible by design.</p>
 *
 * <p>Note these are plain MVC mappings under a hard-coded {@code /actuator} prefix; they are
 * not Spring Boot Actuator endpoints, so {@code management.endpoints.web.exposure.include}
 * and {@code management.server} base-path/port settings have no effect on them.</p>
 */
@RestController
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
    @GetMapping("/pluginDigest")
    public Flux<Map<String, Map<String, Object>>> pluginDigest() {
        return Flux.just(BaseDataCache.getInstance().getPluginMap().entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, entry -> summarizePlugin(entry.getValue()), (a, b) -> a, LinkedHashMap::new)));
    }

    /**
     * Selector data digests flux.
     *
     * @return the flux
     */
    @GetMapping("/selectorDigest")
    public Flux<Map<String, List<Map<String, Object>>>> selectorDigest() {
        return Flux.just(summarizeListGroups(BaseDataCache.getInstance().getSelectorMap(), this::summarizeSelector));
    }

    /**
     * Rule data digests flux.
     *
     * @return the flux
     */
    @GetMapping("/ruleDigest")
    public Flux<Map<String, List<Map<String, Object>>>> ruleDigest() {
        return Flux.just(summarizeListGroups(BaseDataCache.getInstance().getRuleMap(), this::summarizeRule));
    }

    /**
     * selector match cache digests flux.
     *
     * @return the flux
     */
    @GetMapping("/selectorMatchDigest")
    public Flux<Map<String, List<Map<String, Object>>>> getSelectorMatchDigest() {
        return Flux.just(summarizeMapGroups(MatchDataCache.getInstance().getSelectorMatchCache(), this::summarizeSelector));
    }

    /**
     * rule match cache digests flux.
     *
     * @return the flux
     */
    @GetMapping("/ruleMatchDigest")
    public Flux<Map<String, List<Map<String, Object>>>> getRuleMatchDigest() {
        return Flux.just(summarizeMapGroups(MatchDataCache.getInstance().getRuleMatchCache(), this::summarizeRule));
    }

    /**
     * get metadata digests map.
     *
     * @return metadata digest map
     */
    @GetMapping("/metadataDigest")
    public Flux<Map<String, Map<String, Object>>> getMetadataDigest() {
        return Flux.just(summarizeMetadata(MetaDataCache.getInstance().getMetaDataMap()));
    }

    /**
     * get metadata cache digests.
     *
     * @return metadata digest map
     */
    @GetMapping("/metadataCacheDigest")
    public Flux<Map<String, Map<String, Object>>> getMetaDataCacheDigest() {
        return Flux.just(summarizeMetadata(MetaDataCache.getInstance().getMetaDataCache()));
    }

    private Map<String, Object> summarizePlugin(final PluginData data) {
        Map<String, Object> item = new LinkedHashMap<>();
        item.put("enabled", data.getEnabled());
        item.put("role", data.getRole());
        item.put("configDigest", redactor.digest(data.getConfig()));
        return item;
    }

    private Map<String, Object> summarizeSelector(final SelectorData data) {
        Map<String, Object> item = new LinkedHashMap<>();
        item.put("name", data.getName());
        item.put("selectorId", data.getId());
        item.put("handleDigest", redactor.digest(data.getHandle()));
        item.put("conditionCount", size(data.getConditionList()));
        return item;
    }

    private Map<String, Object> summarizeRule(final RuleData data) {
        Map<String, Object> item = new LinkedHashMap<>();
        item.put("name", data.getName());
        item.put("ruleId", data.getId());
        item.put("pluginName", data.getPluginName());
        item.put("handleDigest", redactor.digest(data.getHandle()));
        item.put("conditionCount", size(data.getConditionDataList()));
        return item;
    }

    private <T> Map<String, List<Map<String, Object>>> summarizeListGroups(final Map<String, List<T>> groups, final Function<T, Map<String, Object>> mapper) {
        Map<String, List<Map<String, Object>>> root = new LinkedHashMap<>();
        for (Map.Entry<String, List<T>> entry : groups.entrySet()) {
            List<Map<String, Object>> list = new ArrayList<>(entry.getValue().size());
            for (T item : entry.getValue()) {
                list.add(mapper.apply(item));
            }
            root.put(entry.getKey(), list);
        }
        return root;
    }

    private <T> Map<String, List<Map<String, Object>>> summarizeMapGroups(final Map<String, Map<String, T>> groups, final Function<T, Map<String, Object>> mapper) {
        Map<String, List<Map<String, Object>>> root = new LinkedHashMap<>();
        for (Map.Entry<String, Map<String, T>> entry : groups.entrySet()) {
            List<Map<String, Object>> list = new ArrayList<>(entry.getValue().size());
            for (T item : entry.getValue().values()) {
                list.add(mapper.apply(item));
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
            item.put("path", data.getPath());
            item.put("rpcType", data.getRpcType());
            item.put("method", data.getMethodName());
            item.put("rpcExtDigest", redactor.digest(data.getRpcExt()));
            root.put(entry.getKey(), item);
        }
        return root;
    }

    private int size(final List<?> list) {
        return Objects.isNull(list) ? 0 : list.size();
    }

}
