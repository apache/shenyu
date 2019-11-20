/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.register.dubbo64;

import com.alibaba.dubbo.config.ReferenceConfig;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.cache.Weigher;
import java.util.concurrent.ExecutionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 关于dubbo配置文件的一些缓存信息
 * 保存dubbo的对象引用，目前缓存的最大Size为50000.
 *
 * @author chenbin
 */
public final class DubboReferenceCache {

    private static final Logger LOG = LoggerFactory.getLogger(DubboReferenceCache.class);

    /**
     * The constant INSTANCE.
     */
    public static final DubboReferenceCache INSTANCE = new DubboReferenceCache();

    /**
     * 缓存引用对象5000.
     */
    private final int maxCount = 50000;

    private RegistryReference registryReference;

    /**
     * 缓存初始化.
     */
    private final LoadingCache<DubboPath, ReferenceConfig<?>> cache = CacheBuilder.newBuilder()
            .maximumWeight(maxCount)
            .weigher((Weigher<DubboPath, ReferenceConfig<?>>) (DubboPath, ReferenceConfig) -> getSize())
            .removalListener(notification -> {
                ReferenceConfig config = notification.getValue();
                if (config != null) {
                    config.destroy();
                }
            })
            .build(new CacheLoader<DubboPath, ReferenceConfig<?>>() {
                @Override
                public ReferenceConfig<?> load(DubboPath path) {
                    return buildReference(path);
                }

                private ReferenceConfig buildReference(DubboPath path) {
                    if (path.isRegisterServer()) {
                        ReferenceConfig<?> referenceConfig = new ReferenceConfig<>();
                        referenceConfig.setApplication(registryReference.getApplicationConfig());
                        referenceConfig.setRegistry(registryReference.getRegistryConfig());
                        referenceConfig.setInterface(path.getService());
                        return referenceConfig;
                    }
                    return null;
                }
            });

    private DubboReferenceCache() {
        registryReference = new RegistryReference();
    }

    /**
     * 获取缓存中的size.
     * 这里可能强转，因为只有大于{@link Integer#MAX_VALUE}的时候才会造成类型错误.
     *
     * @return 大小
     */
    private int getSize() {
        return (int) cache.size();
    }

    /**
     * Get reference config.
     *
     * @param path the path
     * @return the reference config
     */
    @SuppressWarnings("all")
    public <T> ReferenceConfig<T> get(DubboPath path) {
        try {
            return (ReferenceConfig<T>) cache.get(path);
        } catch (ExecutionException ignored) {
            return null;
        }
    }

    /**
     * 根据一个dubbo的引用Key初始化服务.
     *
     * @param dubboPath the dubbo path.
     */
    public void init(DubboPath dubboPath) {
        try {
            ReferenceConfig rcf = cache.get(dubboPath);
            if (rcf != null) {
                //调用get方法就是初始化;
                Object obj = rcf.get();
                if (obj != null) {
                    LOG.info("初始化引用成功{}", dubboPath);
                }
            }

        } catch (Exception ex) {
            LOG.warn("初始化引用没有找到提供者【{}】", dubboPath);
            if (LOG.isDebugEnabled()) {
                LOG.debug("", ex);
            }
        }
        cache.invalidate(dubboPath);
    }
}
