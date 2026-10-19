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

package org.apache.shenyu.plugin.grpc.cache;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import io.grpc.MethodDescriptor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.convert.rule.impl.GrpcRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.grpc.resolver.ShenyuServiceInstanceLists;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * Grpc config cache.
 */
public final class ApplicationConfigCache {

    private static final Logger LOG = LoggerFactory.getLogger(ApplicationConfigCache.class);

    private static final GrpcExtInfo DEFAULT_EXT_INFO = new GrpcExtInfo(5000, MethodDescriptor.MethodType.UNARY);

    private final Supplier<CommonHandleCache<String, List<GrpcUpstream>>> grpcUpstreamCachedHandle = new BeanHolder<>(CommonHandleCache::new);

    private final Supplier<CommonHandleCache<String, GrpcRuleHandle>> ruleCachedHandle = new BeanHolder<>(CommonHandleCache::new);

    private final LoadingCache<String, ShenyuServiceInstanceLists> cache = CacheBuilder.newBuilder()
            .maximumSize(Constants.CACHE_MAX_COUNT)
            .build(new CacheLoader<>() {
                @Override
                @NonNull
                public ShenyuServiceInstanceLists load(@NonNull final String key) {
                    return new ShenyuServiceInstanceLists(key);
                }
            });

    private final Cache<String, CachedGrpcExtInfo> grpcExtCache = CacheBuilder.newBuilder()
            .maximumSize(Constants.CACHE_MAX_COUNT)
            .build();

    private final Map<String, Consumer<Object>> watchUpstreamListener = new ConcurrentHashMap<>();

    private ApplicationConfigCache() {
    }

    /**
     * Get shenyuServiceInstanceList.
     *
     * @param contextPath contextPath
     * @return ShenyuServiceInstanceLists instances
     */
    public ShenyuServiceInstanceLists get(final String contextPath) {
        try {
            return cache.get(contextPath);
        } catch (ExecutionException e) {
            throw new ShenyuException(e.getCause());
        }
    }

    /**
     * handlerUpstream.
     *
     * @param selectorId   selectorId
     * @param upstreamList upstreamList
     */
    public void handlerUpstream(final String selectorId, final List<GrpcUpstream> upstreamList) {
        if (CollectionUtils.isEmpty(upstreamList)) {
            invalidate(selectorId);
            return;
        }
        grpcUpstreamCachedHandle.get().cachedHandle(selectorId, upstreamList);
        Consumer<Object> consumer = watchUpstreamListener.get(selectorId);
        if (Objects.nonNull(consumer)) {
            consumer.accept(System.currentTimeMillis());
        }
    }

    /**
     * invalidate client.
     *
     * @param selectorId selectorId
     */
    public void invalidate(final String selectorId) {
        grpcUpstreamCachedHandle.get().removeHandle(selectorId);
        cache.invalidate(selectorId);
        watchUpstreamListener.remove(selectorId);
        ruleCachedHandle.get().removeHandle(CacheKeyUtils.INST.getKey(selectorId, Constants.DEFAULT_RULE));
        GrpcClientCache.removeClient(selectorId);
    }

    /**
     * Refresh.
     *
     * @param key      contextPath
     * @param consumer consumer
     */
    public void watch(final String key, final Consumer<Object> consumer) {
        watchUpstreamListener.put(key, consumer);
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static ApplicationConfigCache getInstance() {
        return ApplicationConfigCacheInstance.INSTANCE;
    }

    /**
     * handlerRule.
     *
     * @param ruleDataKey ruleDataKey
     * @param ruleHandle ruleHandle
     */
    public void cacheRuleHandle(final String ruleDataKey, final String ruleHandle) {
        final String handler = StringUtils.isNotBlank(ruleHandle) ? ruleHandle : "{}";
        ruleCachedHandle.get().cachedHandle(ruleDataKey, GsonUtils.getInstance().fromJson(handler, GrpcRuleHandle.class));
    }

    /**
     * getCacheRuleHandle.
     *
     * @param ruleDataKey ruleDataKey
     * @return {@link GrpcRuleHandle}
     */
    public GrpcRuleHandle getCacheRuleHandle(final String ruleDataKey) {
        return ruleCachedHandle.get().obtainHandle(ruleDataKey);
    }

    /**
     * removeRuleHandle.
     *
     * @param ruleDataKey ruleDataKey
     */
    public void removeRuleHandle(final String ruleDataKey) {
        ruleCachedHandle.get().removeHandle(ruleDataKey);
    }

    /**
     * getGrpcUpstreamListCache.
     *
     * @param selectorId selectorId
     * @return {@link List GrpcUpstream}
     */
    public List<GrpcUpstream> getGrpcUpstreamListCache(final String selectorId) {
        return grpcUpstreamCachedHandle.get().obtainHandle(selectorId);
    }

    /**
     * Obtains or parses GrpcExtInfo for metadata.
     *
     * @param metaData metaData
     * @return parsed or cached GrpcExtInfo
     */
    public GrpcExtInfo obtainGrpcExtInfo(final MetaData metaData) {
        if (Objects.isNull(metaData) || StringUtils.isBlank(metaData.getRpcExt())) {
            return DEFAULT_EXT_INFO;
        }
        final String key = buildCacheKey(metaData);
        final String rpcExt = metaData.getRpcExt();
        CachedGrpcExtInfo cached = grpcExtCache.getIfPresent(key);
        if (Objects.nonNull(cached) && Objects.equals(cached.rpcExt, rpcExt)) {
            return cached.extInfo;
        }
        GrpcExtInfo parsed = parseRpcExt(rpcExt);
        grpcExtCache.put(key, new CachedGrpcExtInfo(rpcExt, parsed));
        return parsed;
    }

    /**
     * Cache GrpcExtInfo for metadata on metadata update.
     *
     * @param metaData metaData
     */
    public void cacheGrpcExtInfo(final MetaData metaData) {
        if (Objects.isNull(metaData)) {
            return;
        }
        final String key = buildCacheKey(metaData);
        final String rpcExt = metaData.getRpcExt();
        if (StringUtils.isBlank(rpcExt)) {
            grpcExtCache.put(key, new CachedGrpcExtInfo(rpcExt, DEFAULT_EXT_INFO));
            return;
        }
        try {
            GrpcExtInfo extInfo = parseRpcExt(rpcExt);
            grpcExtCache.put(key, new CachedGrpcExtInfo(rpcExt, extInfo));
        } catch (Exception e) {
            LOG.warn("Failed to parse gRPC rpcExt for metadata: {}", key, e);
        }
    }

    /**
     * Invalidate GrpcExtInfo for metadata on metadata removal.
     *
     * @param metaData metaData
     */
    public void invalidateGrpcExtInfo(final MetaData metaData) {
        if (Objects.isNull(metaData)) {
            return;
        }
        if (StringUtils.isNotBlank(metaData.getPath())) {
            grpcExtCache.invalidate(metaData.getPath());
        }
        if (StringUtils.isNotBlank(metaData.getId())) {
            grpcExtCache.invalidate(metaData.getId());
        }
        if (StringUtils.isNotBlank(metaData.getServiceName()) && StringUtils.isNotBlank(metaData.getMethodName())) {
            grpcExtCache.invalidate(metaData.getServiceName() + "." + metaData.getMethodName());
        }
    }

    /**
     * Clean GrpcExtInfo cache.
     */
    public void cleanGrpcExtInfo() {
        grpcExtCache.invalidateAll();
    }

    private GrpcExtInfo parseRpcExt(final String rpcExt) {
        if (StringUtils.isBlank(rpcExt)) {
            return DEFAULT_EXT_INFO;
        }
        GrpcExtInfo extInfo = GsonUtils.getGson().fromJson(rpcExt, GrpcExtInfo.class);
        if (Objects.isNull(extInfo)) {
            return DEFAULT_EXT_INFO;
        }
        if (Objects.isNull(extInfo.getTimeout())) {
            extInfo.setTimeout(5000);
        }
        if (Objects.isNull(extInfo.getMethodType())) {
            extInfo.setMethodType(MethodDescriptor.MethodType.UNARY);
        }
        return extInfo;
    }

    private String buildCacheKey(final MetaData metaData) {
        if (StringUtils.isNotBlank(metaData.getPath())) {
            return metaData.getPath();
        }
        if (StringUtils.isNotBlank(metaData.getId())) {
            return metaData.getId();
        }
        return metaData.getServiceName() + "." + metaData.getMethodName();
    }

    static final class CachedGrpcExtInfo {

        private final String rpcExt;

        private final GrpcExtInfo extInfo;

        CachedGrpcExtInfo(final String rpcExt, final GrpcExtInfo extInfo) {
            this.rpcExt = rpcExt;
            this.extInfo = extInfo;
        }
    }


    /**
     * The type Application config cache instance.
     */
    static final class ApplicationConfigCacheInstance {
        /**
         * The Instance.
         */
        static final ApplicationConfigCache INSTANCE = new ApplicationConfigCache();

        private ApplicationConfigCacheInstance() {

        }
    }
}
