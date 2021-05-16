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

package org.apache.shenyu.plugin.apache.dubbo.cache;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import lombok.extern.slf4j.Slf4j;
import org.apache.dubbo.config.ReferenceConfig;
import org.apache.dubbo.rpc.Invoker;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.ReflectUtils;

import java.util.Optional;
import java.util.concurrent.ExecutionException;

/**
 * Cache Dubbo provider version.
 */
@Slf4j
public final class DubboProviderVersionCache {

    private final int maxCount = 1000;

    private final LoadingCache<String, String> cache = CacheBuilder.newBuilder()
            .maximumSize(maxCount)
            .build(new CacheLoader<String, String>() {
                @Override
                public String load(final String key) {
                    return "";
                }
            });

    private DubboProviderVersionCache() {
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static DubboProviderVersionCache getInstance() {
        return DubboProviderVersionCache.DubboProviderVersionCacheInstance.INSTANCE;
    }

    /**
     * cache dubbo provider version.
     *
     * @param path the path
     * @param reference the dubbo reference
     */
    public void cacheProviderVersion(final String path, final ReferenceConfig reference) {
        String version;
        try {
            Invoker invoker = (Invoker) ReflectUtils.getFieldValue(reference, Constants.DUBBO_REFRENCE_INVOKER);
            version = Optional.ofNullable(invoker.getUrl().getParameter(Constants.DUBBO_PROVIDER_VERSION)).orElse("");
        } catch (Exception e) {
            version = "";
        }
        cache.put(path, version);
    }

    /**
     * Get provider version.
     *
     * @param path the path
     * @return the reference config
     */
    public String get(final String path) {
        try {
            return cache.get(path);
        } catch (ExecutionException e) {
            throw new ShenyuException(e.getCause());
        }
    }

    /**
     * Invalidate.
     *
     * @param path the path
     */
    public void invalidate(final String path) {
        cache.invalidate(path);
    }

    /**
     * Invalidate all.
     */
    public void invalidateAll() {
        cache.invalidateAll();
    }

    /**
     * The Dubbo provider version instance.
     */
    static class DubboProviderVersionCacheInstance {
        /**
         * The Instance.
         */
        static final DubboProviderVersionCache INSTANCE = new DubboProviderVersionCache();
    }
}
