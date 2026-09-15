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

package org.apache.shenyu.plugin.divide.handler;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.plugin.base.handler.DiscoveryUpstreamDataHandler;
import org.springframework.util.ObjectUtils;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * upstreamList data change.
 */
public class DivideUpstreamDataHandler implements DiscoveryUpstreamDataHandler {

    @Override
    public void handlerDiscoveryUpstreamData(final DiscoverySyncData discoverySyncData) {
        if (Objects.isNull(discoverySyncData) || Objects.isNull(discoverySyncData.getSelectorId())) {
            return;
        }
        List<DiscoveryUpstreamData> upstreamList = discoverySyncData.getUpstreamDataList();
        final List<Upstream> upstreams = convertUpstreamList(upstreamList);
        final List<Upstream> grayUpstreamList = upstreams.stream().filter(Upstream::isGray).toList();
        if (!grayUpstreamList.isEmpty()) {
            UpstreamCacheManager.getInstance().submit(discoverySyncData.getSelectorId(), grayUpstreamList);
        } else {
            UpstreamCacheManager.getInstance().submit(discoverySyncData.getSelectorId(), upstreams);
        }
        // the update is also need to clean, but there is no way to
        // distinguish between crate and update, so it is always clean
        MetaDataCache.getInstance().clean();
    }

    @Override
    public String pluginName() {
        return PluginEnum.DIVIDE.getName();
    }

    private List<Upstream> convertUpstreamList(final List<DiscoveryUpstreamData> upstreamList) {
        if (ObjectUtils.isEmpty(upstreamList)) {
            return Collections.emptyList();
        }
        return upstreamList.stream().map(u -> {
            JsonObject properties = Optional.ofNullable(u.getProps())
                    .map(ps -> GsonUtils.getInstance().fromJson(ps, JsonObject.class)).orElseGet(JsonObject::new);
            Map<String, String> labels = readLabels(properties);
            Upstream upstream = Upstream.builder()
                    .protocol(u.getProtocol())
                    .url(u.getUrl())
                    .weight(u.getWeight())
                    .warmup(Integer.parseInt(readScalar(properties, "warmup", "10")))
                    .gray(readBoolean(properties, "gray", false))
                    .healthCheckEnabled(readBoolean(properties, "healthCheckEnabled", true))
                    .status(0 == u.getStatus())
                    .timestamp(Optional.ofNullable(u.getDateCreated()).map(Timestamp::getTime).orElse(System.currentTimeMillis()))
                    .build();
            upstream.setMetadata(labels);
            return upstream;
        }).collect(Collectors.toList());
    }

    private String readScalar(final JsonObject properties, final String name, final String defaultValue) {
        JsonElement value = properties.get(name);
        if (Objects.isNull(value)) {
            return defaultValue;
        }
        if (!value.isJsonPrimitive()) {
            throw new IllegalArgumentException("Upstream property " + name + " must be a scalar");
        }
        return value.getAsString();
    }

    private boolean readBoolean(final JsonObject properties, final String name, final boolean defaultValue) {
        String value = readScalar(properties, name, Boolean.toString(defaultValue));
        if (!"true".equalsIgnoreCase(value) && !"false".equalsIgnoreCase(value)) {
            throw new IllegalArgumentException("Upstream property " + name + " must be a boolean");
        }
        return Boolean.parseBoolean(value);
    }

    private Map<String, String> readLabels(final JsonObject properties) {
        JsonElement labels = properties.get("labels");
        if (Objects.isNull(labels)) {
            return Collections.emptyMap();
        }
        if (!labels.isJsonObject()) {
            throw new IllegalArgumentException("Upstream labels must be an object of strings");
        }
        Map<String, String> result = new HashMap<>();
        for (Map.Entry<String, JsonElement> entry : labels.getAsJsonObject().entrySet()) {
            JsonElement value = entry.getValue();
            if (!(value instanceof JsonPrimitive) || !value.getAsJsonPrimitive().isString()) {
                throw new IllegalArgumentException("Upstream label values must be strings");
            }
            result.put(entry.getKey(), value.getAsString());
        }
        return Collections.unmodifiableMap(result);
    }

}
