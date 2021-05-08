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
import com.google.common.cache.Weigher;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.exception.SoulException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.grpc.resolver.SoulServiceInstance;
import org.apache.shenyu.plugin.grpc.resolver.SoulServiceInstanceLists;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * Grpc config cache.
 *
 * @author zhanglei
 */
@Slf4j
public final class ApplicationConfigCache {

    private final int maxCount = 50000;

    private final LoadingCache<String, SoulServiceInstanceLists> cache = CacheBuilder.newBuilder()
            .maximumWeight(maxCount)
            .weigher((Weigher<String, SoulServiceInstanceLists>) (string, referenceConfig) -> getSize())
            .build(new CacheLoader<String, SoulServiceInstanceLists>() {
                @Override
                public SoulServiceInstanceLists load(final String key) {
                    return new SoulServiceInstanceLists(new CopyOnWriteArrayList<>(), key);
                }
            });

    private final Map<String, Consumer<Object>> listener = new ConcurrentHashMap<>();

    private ApplicationConfigCache() {
    }

    private int getSize() {
        return (int) cache.size();
    }

    /**
     * Get soulServiceInstanceList.
     *
     * @param contextPath contextPath
     * @return SoulServiceInstanceLists instances
     */
    public SoulServiceInstanceLists get(final String contextPath) {
        try {
            return cache.get(contextPath);
        } catch (ExecutionException e) {
            throw new SoulException(e.getCause());
        }
    }

    /**
     * Init prx.
     *
     * @param selectorData selectorData
     */
    public void initPrx(final SelectorData selectorData) {
        try {
            final List<DivideUpstream> upstreamList = GsonUtils.getInstance().fromList(selectorData.getHandle(), DivideUpstream.class);
            if (null == upstreamList || upstreamList.size() == 0) {
                invalidate(selectorData.getName());
                return;
            }
            SoulServiceInstanceLists soulServiceInstances = cache.get(selectorData.getName());
            List<SoulServiceInstance> instances = soulServiceInstances.getSoulServiceInstances();
            instances.clear();
            instances.addAll(upstreamList.stream().map(this::build).collect(Collectors.toList()));
            Consumer<Object> consumer = listener.get(selectorData.getName());
            if (Objects.nonNull(consumer)) {
                consumer.accept(System.currentTimeMillis());
            }
        } catch (ExecutionException e) {
            throw new SoulException(e.getCause());
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

    private SoulServiceInstance build(final DivideUpstream divideUpstream) {
        String[] ipAndPort = divideUpstream.getUpstreamUrl().split(":");
        SoulServiceInstance instance = new SoulServiceInstance(ipAndPort[0], Integer.parseInt(ipAndPort[1]));
        instance.setWeight(divideUpstream.getWeight());
        instance.setStatus(divideUpstream.isStatus());
        return instance;
    }

    /**
     * The type Application config cache instance.
     */
    static class ApplicationConfigCacheInstance {
        /**
         * The Instance.
         */
        static final ApplicationConfigCache INSTANCE = new ApplicationConfigCache();
    }
}
