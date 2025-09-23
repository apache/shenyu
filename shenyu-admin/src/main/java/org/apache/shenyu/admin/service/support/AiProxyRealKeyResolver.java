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

package org.apache.shenyu.admin.service.support;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * AiProxyRealKeyResolver resolves real api key from selector handle JSON.
 * Event-driven invalidation, single-flight resolve, no time TTL by default.
 */
@Component
public class AiProxyRealKeyResolver {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyRealKeyResolver.class);

    private static final long RESOLVE_TIMEOUT_SECONDS = 2L;

    private final SelectorMapper selectorMapper;

    private final ObjectMapper objectMapper;

    /** selectorId -> cached real key (nullable). */
    private final ConcurrentHashMap<String, AtomicReference<String>> cache = new ConcurrentHashMap<>();

    /** selectorId -> in-flight resolving future. */
    private final Map<String, CompletableFuture<String>> inFlight = new ConcurrentHashMap<>();

    public AiProxyRealKeyResolver(final SelectorMapper selectorMapper, final ObjectMapper objectMapper) {
        this.selectorMapper = selectorMapper;
        this.objectMapper = objectMapper;
    }

    /**
     * Resolve real api key by selector id with single-flight.
     * @param selectorId selector id
     * @return optional real api key
     */
    public Optional<String> resolveRealKey(final String selectorId) {
        if (StringUtils.isBlank(selectorId)) {
            return Optional.empty();
        }
        final AtomicReference<String> ref = cache.get(selectorId);
        if (Objects.nonNull(ref)) {
            final String v = ref.get();
            LOG.debug("[AiProxyRealKeyResolver] cache hit selectorId={}, masked={}...", selectorId, mask(v));
            return Optional.ofNullable(ref.get());
        }
        // single-flight: only one resolving task per selectorId
        final CompletableFuture<String> future = inFlight.computeIfAbsent(selectorId, id ->
                CompletableFuture.supplyAsync(() -> doResolve(id))
                        .whenComplete((val, ex) -> {
                            try {
                                cache.put(id, new AtomicReference<>(val));
                                if (Objects.nonNull(ex)) {
                                    LOG.warn("[AiProxyRealKeyResolver] resolve failed for selectorId={}: {}", id, ex.getMessage());
                                } else {
                                    LOG.info("[AiProxyRealKeyResolver] resolved selectorId={}, masked={}...", id, mask(val));
                                }
                            } finally {
                                inFlight.remove(id);
                            }
                        })
        );
        try {
            return Optional.ofNullable(future.get(RESOLVE_TIMEOUT_SECONDS, TimeUnit.SECONDS));
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            LOG.warn("[AiProxyRealKeyResolver] resolve interrupted for selectorId={}", selectorId);
            return Optional.empty();
        } catch (TimeoutException ex) {
            LOG.warn("[AiProxyRealKeyResolver] resolve timed out after {}s for selectorId={}", RESOLVE_TIMEOUT_SECONDS, selectorId);
            return Optional.empty();
        } catch (ExecutionException ex) {
            LOG.warn("[AiProxyRealKeyResolver] resolve failed for selectorId={}: {}", selectorId, Objects.nonNull(ex.getCause()) ? ex.getCause().getMessage() : ex.getMessage());
            return Optional.empty();
        }
    }

    /**
     * Invalidate cache and in-flight for a selector.
     * @param selectorId selector id
     */
    public void invalidate(final String selectorId) {
        if (StringUtils.isNotBlank(selectorId)) {
            cache.remove(selectorId);
            final CompletableFuture<String> f = inFlight.remove(selectorId);
            if (Objects.nonNull(f)) {
                f.cancel(true);
            }
        }
    }

    /**
     * Force refresh (invalidate then resolve once).
     * @param selectorId selector id
     * @return optional real api key
     */
    public Optional<String> refresh(final String selectorId) {
        invalidate(selectorId);
        return resolveRealKey(selectorId);
    }

    private String doResolve(final String selectorId) {
        try {
            final SelectorDO selector = selectorMapper.selectById(selectorId);
            if (Objects.isNull(selector) || StringUtils.isBlank(selector.getHandle())) {
                return null;
            }
            return extractApiKey(selector.getHandle());
        } catch (Exception e) {
            return null;
        }
    }

    private String extractApiKey(final String handleJson) {
        try {
            final JsonNode root = objectMapper.readTree(handleJson);
            final JsonNode direct = root.get("apiKey");
            if (Objects.nonNull(direct) && direct.isTextual()) {
                return direct.asText();
            }
            return findFirstApiKey(root);
        } catch (Exception e) {
            return null;
        }
    }

    private String findFirstApiKey(final JsonNode node) {
        if (Objects.isNull(node)) {
            return null;
        }
        if (node.isObject()) {
            final JsonNode apiKey = node.get("apiKey");
            if (Objects.nonNull(apiKey) && apiKey.isTextual()) {
                return apiKey.asText();
            }
            final var fields = node.fields();
            while (fields.hasNext()) {
                final var entry = fields.next();
                final String found = findFirstApiKey(entry.getValue());
                if (StringUtils.isNotBlank(found)) {
                    return found;
                }
            }
        } else if (node.isArray()) {
            for (JsonNode child : node) {
                final String found = findFirstApiKey(child);
                if (StringUtils.isNotBlank(found)) {
                    return found;
                }
            }
        }
        return null;
    }

    private String mask(final String v) {
        if (StringUtils.isBlank(v)) {
            return "null";
        }
        return v.substring(0, Math.min(6, v.length()));
    }
} 