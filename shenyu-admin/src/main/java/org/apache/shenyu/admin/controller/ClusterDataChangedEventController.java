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

package org.apache.shenyu.admin.controller;

import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.model.dto.ClusterDataChangedEventPayload;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.Nullable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Objects;

/**
 * Receives data change events handed over by non-master admin nodes and
 * re-publishes them locally, so configuration committed on any node reaches
 * the listeners (websocket push, long polling cache, registry writers) that
 * run on the current master.
 *
 * <p>Both envelope versions are accepted: version 1 carries a serialized JSON
 * string in {@code source}, version 2 carries a structured array in
 * {@code structuredSource}. Senders only use version 2 after the master accepted
 * it, so a mixed-version cluster keeps working.
 */
@RestController
@RequestMapping("/cluster")
@ConditionalOnProperty(value = "shenyu.cluster.enabled", havingValue = "true")
public class ClusterDataChangedEventController {

    private static final int LEGACY_PAYLOAD_VERSION = 1;

    private static final int STRUCTURED_PAYLOAD_VERSION = 2;

    private final ApplicationEventPublisher eventPublisher;

    private final ClusterSelectMasterService clusterSelectMasterService;

    private final ObjectMapper objectMapper;

    /**
     * Instantiates a new cluster data changed event controller.
     *
     * @param eventPublisher             the application event publisher
     * @param clusterSelectMasterService the cluster select master service
     * @param objectMapper               the mapper used to read the structured source
     */
    @Autowired
    public ClusterDataChangedEventController(final ApplicationEventPublisher eventPublisher,
                                             @Nullable final ClusterSelectMasterService clusterSelectMasterService,
                                             final ObjectMapper objectMapper) {
        this.eventPublisher = eventPublisher;
        this.clusterSelectMasterService = clusterSelectMasterService;
        this.objectMapper = objectMapper;
    }

    /**
     * Accept a data change event forwarded by a non-master admin node and
     * re-publish it locally. Only the current master accepts events; other
     * nodes reject the request so the event can be retried by the sender or
     * picked up by the new master.
     *
     * @param payload the forwarded event payload
     * @return the acceptance result
     */
    @PostMapping("/data-change-event")
    public ResponseEntity<ShenyuAdminResult> receive(@RequestBody final ClusterDataChangedEventPayload payload) {
        if (Objects.isNull(clusterSelectMasterService) || !clusterSelectMasterService.isMaster()) {
            return ResponseEntity.status(HttpStatus.CONFLICT)
                    .body(ShenyuAdminResult.error("this node is not the cluster master, data change event not accepted"));
        }
        if (Objects.isNull(payload) || Objects.isNull(payload.getGroupKey())
                || Objects.isNull(payload.getEventType())
                || (Objects.isNull(payload.getSource()) && Objects.isNull(payload.getStructuredSource()))) {
            return ResponseEntity.badRequest().body(ShenyuAdminResult.error("event fields must not be null"));
        }
        if (Objects.nonNull(payload.getSource()) && Objects.nonNull(payload.getStructuredSource())) {
            return ResponseEntity.badRequest()
                    .body(ShenyuAdminResult.error("event source must use exactly one representation"));
        }
        final ConfigGroupEnum groupKey;
        final DataEventTypeEnum eventType;
        try {
            groupKey = ConfigGroupEnum.valueOf(payload.getGroupKey());
            eventType = DataEventTypeEnum.valueOf(payload.getEventType());
        } catch (RuntimeException ex) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(ShenyuAdminResult.error("unknown data change event group or type: "
                            + payload.getGroupKey() + "/" + payload.getEventType()));
        }
        final List<?> source;
        try {
            source = resolveSource(payload, groupKey);
            if (Objects.isNull(source) || source.stream().anyMatch(Objects::isNull)) {
                return ResponseEntity.badRequest().body(ShenyuAdminResult.error("source must be an array of non-null records"));
            }
        } catch (RuntimeException ex) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(ShenyuAdminResult.error("invalid source for data change event group: " + groupKey.name()));
        }
        eventPublisher.publishEvent(new DataChangedEvent(groupKey, eventType, source, payload.getNamespaceId()));
        return ResponseEntity.ok(ShenyuAdminResult.success());
    }

    private List<?> resolveSource(final ClusterDataChangedEventPayload payload, final ConfigGroupEnum groupKey) {
        final Integer version = payload.getPayloadVersion();
        if (Objects.nonNull(payload.getStructuredSource())) {
            if (Objects.nonNull(version) && STRUCTURED_PAYLOAD_VERSION != version) {
                throw new IllegalArgumentException("unsupported structured payload version: " + version);
            }
            return deserializeStructuredSource(groupKey, payload.getStructuredSource());
        }
        if (Objects.nonNull(version) && LEGACY_PAYLOAD_VERSION != version) {
            throw new IllegalArgumentException("unsupported payload version: " + version);
        }
        return deserializeSource(groupKey, payload.getSource());
    }

    private List<?> deserializeStructuredSource(final ConfigGroupEnum groupKey, final JsonNode sourceJson) {
        if (!sourceJson.isArray()) {
            throw new IllegalArgumentException("structured source must be an array");
        }
        final JavaType listType = objectMapper.getTypeFactory()
                .constructCollectionType(List.class, resolveSourceClass(groupKey));
        return objectMapper.convertValue(sourceJson, listType);
    }

    private List<?> deserializeSource(final ConfigGroupEnum groupKey, final String sourceJson) {
        return GsonUtils.getInstance().fromList(sourceJson, resolveSourceClass(groupKey));
    }

    private Class<?> resolveSourceClass(final ConfigGroupEnum groupKey) {
        final Class<?> targetClass;
        switch (groupKey) {
            case APP_AUTH:
                targetClass = AppAuthData.class;
                break;
            case PLUGIN:
                targetClass = PluginData.class;
                break;
            case RULE:
                targetClass = RuleData.class;
                break;
            case SELECTOR:
                targetClass = SelectorData.class;
                break;
            case META_DATA:
                targetClass = MetaData.class;
                break;
            case PROXY_SELECTOR:
                targetClass = ProxySelectorData.class;
                break;
            case AI_PROXY_API_KEY:
                targetClass = ProxyApiKeyData.class;
                break;
            case DISCOVER_UPSTREAM:
                targetClass = DiscoverySyncData.class;
                break;
            default:
                throw new IllegalArgumentException("Unexpected value: " + groupKey);
        }
        return targetClass;
    }
}
