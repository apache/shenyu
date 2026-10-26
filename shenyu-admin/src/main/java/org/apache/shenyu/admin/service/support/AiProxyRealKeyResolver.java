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
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import jakarta.annotation.PreDestroy;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * AiProxyRealKeyResolver resolves real api key from selector handle JSON.
 * Bounded caching with event-driven invalidation, single-flight resolve and a short failure cooldown.
 */
@Component
public class AiProxyRealKeyResolver {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyRealKeyResolver.class);

    private static final long RESOLVE_TIMEOUT_SECONDS = 2L;

    private static final int RESOLVER_THREADS = 10;

    private static final int RESOLVE_QUEUE_CAPACITY = 1024;

    private static final long CACHE_MAXIMUM_SIZE = 10000L;

    private static final long FAILURE_CACHE_SECONDS = 5L;

    private final SelectorMapper selectorMapper;

    private final ObjectMapper objectMapper;

    /** selectorId -> cached real key. */
    private final Cache<String, String> cache = Caffeine.newBuilder().maximumSize(CACHE_MAXIMUM_SIZE).build();

    /** Failed or empty resolutions are retried after a short cooldown. */
    private final Cache<String, Boolean> failures = Caffeine.newBuilder().maximumSize(CACHE_MAXIMUM_SIZE)
            .expireAfterWrite(FAILURE_CACHE_SECONDS, TimeUnit.SECONDS).build();

    /** selectorId -> in-flight resolving future, guarded by its own monitor. */
    private final Map<String, CompletableFuture<String>> inFlight = new HashMap<>();

    private final ExecutorService executorService;

    public AiProxyRealKeyResolver(final SelectorMapper selectorMapper, final ObjectMapper objectMapper) {
        this.selectorMapper = selectorMapper;
        this.objectMapper = objectMapper;
        this.executorService = new ThreadPoolExecutor(RESOLVER_THREADS, RESOLVER_THREADS, 0L, TimeUnit.MILLISECONDS,
                new ArrayBlockingQueue<>(RESOLVE_QUEUE_CAPACITY), ShenyuThreadFactory.create("ai-proxy-key-resolver", false),
                new ThreadPoolExecutor.AbortPolicy());
    }

    /**
     * Resolve real api key by selector id with single-flight.
     * 
     * @param selectorId selector id
     * @return optional real api key
     */
    public Optional<String> resolveRealKey(final String selectorId) {
        if (StringUtils.isBlank(selectorId)) {
            return Optional.empty();
        }
        final CompletableFuture<String> future = getOrStartResolution(selectorId);
        try {
            return Optional.ofNullable(future.get(RESOLVE_TIMEOUT_SECONDS, TimeUnit.SECONDS));
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            LOG.warn("[AiProxyRealKeyResolver] resolve interrupted for selectorId={}", selectorId);
            return Optional.empty();
        } catch (TimeoutException ex) {
            LOG.warn("[AiProxyRealKeyResolver] resolve timed out after {}s for selectorId={}", RESOLVE_TIMEOUT_SECONDS,
                    selectorId);
            return Optional.empty();
        } catch (ExecutionException ex) {
            LOG.warn("[AiProxyRealKeyResolver] resolve failed for selectorId={}: {}", selectorId,
                    Objects.nonNull(ex.getCause()) ? ex.getCause().getMessage() : ex.getMessage());
            return Optional.empty();
        } catch (CancellationException ex) {
            return Optional.empty();
        }
    }

    private CompletableFuture<String> getOrStartResolution(final String selectorId) {
        synchronized (inFlight) {
            final String cached = cache.getIfPresent(selectorId);
            if (Objects.nonNull(cached)) {
                LOG.debug("[AiProxyRealKeyResolver] cache hit selectorId={}, masked={}...", selectorId, mask(cached));
                return CompletableFuture.completedFuture(cached);
            }
            if (Objects.nonNull(failures.getIfPresent(selectorId))) {
                return CompletableFuture.completedFuture(null);
            }
            final CompletableFuture<String> existing = inFlight.get(selectorId);
            if (Objects.nonNull(existing)) {
                return existing;
            }
            // Register before submitting so even immediate completion can remove the flight safely.
            final CompletableFuture<String> future = new CompletableFuture<>();
            inFlight.put(selectorId, future);
            try {
                CompletableFuture.supplyAsync(() -> doResolve(selectorId), executorService)
                        .whenComplete((value, error) -> completeResolution(selectorId, future, value, error));
            } catch (RejectedExecutionException ex) {
                completeResolution(selectorId, future, null, ex);
            }
            return future;
        }
    }

    private void completeResolution(final String selectorId, final CompletableFuture<String> future, final String value, final Throwable error) {
        synchronized (inFlight) {
            // An invalidated task must neither overwrite the cache nor remove a newer flight.
            if (inFlight.remove(selectorId, future)) {
                cacheResult(selectorId, Objects.isNull(error) ? value : null);
                if (Objects.isNull(error)) {
                    LOG.info("[AiProxyRealKeyResolver] resolved selectorId={}, masked={}...", selectorId, mask(value));
                }
            }
            if (Objects.isNull(error)) {
                future.complete(value);
            } else {
                future.completeExceptionally(error);
            }
        }
    }

    private void cacheResult(final String selectorId, final String value) {
        if (Objects.isNull(value)) {
            failures.put(selectorId, Boolean.TRUE);
        } else {
            cache.put(selectorId, value);
            failures.invalidate(selectorId);
        }
    }

    /**
     * Invalidate cache and in-flight for a selector.
     * 
     * @param selectorId selector id
     */
    public void invalidate(final String selectorId) {
        if (StringUtils.isNotBlank(selectorId)) {
            synchronized (inFlight) {
                cache.invalidate(selectorId);
                failures.invalidate(selectorId);
                final CompletableFuture<String> f = inFlight.remove(selectorId);
                if (Objects.nonNull(f)) {
                    f.cancel(true);
                }
            }
        }
    }

    /**
     * Force refresh (invalidate then resolve once).
     * 
     * @param selectorId selector id
     * @return optional real api key
     */
    public Optional<String> refresh(final String selectorId) {
        invalidate(selectorId);
        return resolveRealKey(selectorId);
    }

    private String doResolve(final String selectorId) {
        final SelectorDO selector = selectorMapper.selectById(selectorId);
        if (Objects.isNull(selector) || StringUtils.isBlank(selector.getHandle())) {
            return null;
        }
        return extractApiKey(selector.getHandle());
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

    /**
     * Batch resolve real api keys.
     *
     * @param selectorIds set of selector ids
     * @return map of selectorId -> real api key
     */
    public Map<String, String> resolveRealKeys(final Collection<String> selectorIds) {
        if (Objects.isNull(selectorIds) || selectorIds.isEmpty()) {
            return Collections.emptyMap();
        }
        final Map<String, String> result = new HashMap<>();
        final Set<String> missing = new HashSet<>();

        for (String id : selectorIds) {
            String value = cache.getIfPresent(id);
            if (Objects.nonNull(value)) {
                result.put(id, value);
            } else if (Objects.nonNull(failures.getIfPresent(id))) {
                result.put(id, null);
            } else {
                missing.add(id);
            }
        }

        if (!missing.isEmpty()) {
            java.util.List<SelectorDO> selectors = selectorMapper.selectByIdSet(missing);
            if (Objects.nonNull(selectors)) {
                for (SelectorDO selector : selectors) {
                    String apiKey = extractApiKey(selector.getHandle());
                    cacheResult(selector.getId(), apiKey);
                    result.put(selector.getId(), apiKey);
                }
            }
            // Briefly cache missing IDs, allowing recovery without a selector change event.
            for (String id : missing) {
                if (!result.containsKey(id)) {
                    cacheResult(id, null);
                    result.put(id, null);
                }
            }
        }
        return result;
    }

    /**
     * Shutdown executor service when bean is destroyed.
     */
    @PreDestroy
    public void destroy() {
        executorService.shutdown();
        try {
            if (!executorService.awaitTermination(5, TimeUnit.SECONDS)) {
                executorService.shutdownNow();
            }
        } catch (InterruptedException e) {
            executorService.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

    private String mask(final String v) {
        if (StringUtils.isBlank(v)) {
            return "null";
        }
        return v.substring(0, Math.min(6, v.length()));
    }
}
