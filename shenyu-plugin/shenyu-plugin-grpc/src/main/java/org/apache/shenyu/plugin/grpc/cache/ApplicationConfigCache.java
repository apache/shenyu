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

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.grpc.resolver.ShenyuServiceInstance;
import org.apache.shenyu.plugin.grpc.resolver.ShenyuServiceInstanceLists;
import org.springframework.lang.NonNull;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * Grpc config cache.
 */
public final class ApplicationConfigCache {
    
    private final LoadingCache<String, ShenyuServiceInstanceLists> cache = CacheBuilder.newBuilder()
            .maximumSize(Constants.CACHE_MAX_COUNT)
            .build(new CacheLoader<String, ShenyuServiceInstanceLists>() {
                @Override
                @NonNull
                public ShenyuServiceInstanceLists load(@NonNull final String key) {
                    return new ShenyuServiceInstanceLists(key);
                }
            });
    
    private final Map<String, Consumer<Object>> listener = new ConcurrentHashMap<>();
    
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
     * Init prx.
     *
     * @param selectorData selectorData
     */
    public void initPrx(final SelectorData selectorData) {
        try {
            final List<GrpcUpstream> upstreamList = GsonUtils.getInstance().fromList(selectorData.getHandle(), GrpcUpstream.class);
            if (CollectionUtils.isEmpty(upstreamList)) {
                invalidate(selectorData.getName());
                return;
            }
            ShenyuServiceInstanceLists shenyuServiceInstances = cache.get(selectorData.getName());
            List<ShenyuServiceInstance> instances = shenyuServiceInstances.getShenyuServiceInstances();
            instances.clear();
            instances.addAll(upstreamList.stream().map(this::build).collect(Collectors.toList()));
            Consumer<Object> consumer = listener.get(selectorData.getName());
            if (Objects.nonNull(consumer)) {
                consumer.accept(System.currentTimeMillis());
            }
        } catch (ExecutionException e) {
            throw new ShenyuException(e.getCause());
        }
    }
    
    /**
     * invalidate client.
     *
     * @param contextPath contextPath
     */
    public void invalidate(final String contextPath) {
        cache.invalidate(contextPath);
        listener.remove(contextPath);
        GrpcClientCache.removeClient(contextPath);
    }
    
    /**
     * Refresh.
     *
     * @param key      contextPath
     * @param consumer consumer
     */
    public void watch(final String key, final Consumer<Object> consumer) {
        listener.put(key, consumer);
    }
    
    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static ApplicationConfigCache getInstance() {
        return ApplicationConfigCacheInstance.INSTANCE;
    }
    
    private ShenyuServiceInstance build(final GrpcUpstream grpcUpstream) {
        String[] ipAndPort = grpcUpstream.getUpstreamUrl().split(":");
        ShenyuServiceInstance instance = new ShenyuServiceInstance(ipAndPort[0], Integer.parseInt(ipAndPort[1]));
        instance.setWeight(grpcUpstream.getWeight());
        instance.setStatus(grpcUpstream.isStatus());
        return instance;
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
