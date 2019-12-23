/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.dubbo;

import com.alibaba.dubbo.config.ApplicationConfig;
import com.alibaba.dubbo.config.ReferenceConfig;
import com.alibaba.dubbo.config.RegistryConfig;
import com.alibaba.dubbo.rpc.service.GenericService;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.cache.Weigher;
import org.apache.commons.lang3.StringUtils;

import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.LoadBalanceEnum;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ExecutionException;


/**
 * The type Application config cache.
 */
@SuppressWarnings("all")
public final class ApplicationConfigCache {

    private static final Logger LOG = LoggerFactory.getLogger(ApplicationConfigCache.class);

    private ApplicationConfig applicationConfig;

    private RegistryConfig registryConfig;

    private final int maxCount = 50000;

    private final LoadingCache<String, ReferenceConfig<GenericService>> cache = CacheBuilder.newBuilder()
            .maximumWeight(maxCount)
            .weigher((Weigher<String, ReferenceConfig<GenericService>>) (string, ReferenceConfig) -> getSize())
            .removalListener(notification -> {
                ReferenceConfig config = notification.getValue();
                if (config != null) {
                    try {
                        Class cz = config.getClass();
                        Field field = cz.getDeclaredField("ref");
                        field.setAccessible(true);
                        field.set(config, null);
                        //跟改配置之后dubbo 销毁该实例,但是未置空,如果不处理,重新初始化的时候将获取到NULL照成空指针问题.
                    } catch (Exception e) {
                        LOG.error("修改ref为null", e);
                    }
                }
            })
            .build(new CacheLoader<String, ReferenceConfig<GenericService>>() {
                @Override
                public ReferenceConfig<GenericService> load(final String key) {
                    return new ReferenceConfig<>();
                }
            });

    private ApplicationConfigCache() {
    }

    private int getSize() {
        return (int) cache.size();
    }

    /**
     * 获取ApplicationConfigCache对象.
     *
     * @return 对象 instance
     */
    public static ApplicationConfigCache getInstance() {
        return ApplicationConfigCacheInstance.INSTANCE;
    }

    /**
     * Init.
     *
     * @param register the register
     */
    public void init(final String register) {
        if (applicationConfig == null) {
            applicationConfig = new ApplicationConfig("soul_proxy");
        }
        if (registryConfig == null) {
            registryConfig = new RegistryConfig();
            registryConfig.setProtocol("dubbo");
            registryConfig.setId("soul_proxy");
            registryConfig.setRegister(false);
            registryConfig.setAddress(register);
        }
    }

    /**
     * Init ref reference config.
     *
     * @param metaData the meta data
     * @return the reference config
     */
    public ReferenceConfig<GenericService> initRef(final MetaData metaData) {
        try {
            ReferenceConfig<GenericService> referenceConfig = cache.get(metaData.getServiceName());
            if (StringUtils.isNoneBlank(referenceConfig.getInterface())) {
                return referenceConfig;
            }
        } catch (Exception e) {
            LOG.error("init dubbo ref ex:{}", e.getMessage());
        }
        return build(metaData);

    }

    /**
     * Build reference config.
     *
     * @param metaData the meta data
     * @return the reference config
     */
    public ReferenceConfig<GenericService> build(final MetaData metaData) {

        ReferenceConfig<GenericService> reference = new ReferenceConfig<>();

        reference.setGeneric(true);

        reference.setApplication(applicationConfig);

        reference.setRegistry(registryConfig);

        reference.setInterface(metaData.getServiceName());

        reference.setProtocol("dubbo");

        String rpcExt = metaData.getRpcExt();

        try {
            DubboParamExt dubboParamExt = GsonUtils.getInstance().fromJson(rpcExt, DubboParamExt.class);
            if (Objects.nonNull(dubboParamExt)) {
                if (StringUtils.isNoneBlank(dubboParamExt.getVersion())) {
                    reference.setVersion(dubboParamExt.getVersion());
                }
                if (StringUtils.isNoneBlank(dubboParamExt.getGroup())) {
                    reference.setGroup(dubboParamExt.getGroup());
                }
                if (StringUtils.isNoneBlank(dubboParamExt.getLoadbalance())) {
                    final String loadBalance = dubboParamExt.getLoadbalance();
                    if (LoadBalanceEnum.HASH.getName().equals(loadBalance) || "consistenthash".equals(loadBalance)) {
                        reference.setLoadbalance("consistenthash");
                    } else if (LoadBalanceEnum.ROUND_ROBIN.getName().equals(loadBalance)) {
                        reference.setLoadbalance("roundrobin");
                    } else {
                        reference.setLoadbalance(loadBalance);
                    }
                }
                Optional.ofNullable(dubboParamExt.getTimeout()).ifPresent(reference::setTimeout);
                Optional.ofNullable(dubboParamExt.getRetries()).ifPresent(reference::setRetries);
            }
        } catch (Exception e) {
            LOG.error("rpc 扩展参数转成json异常,{}", metaData);
        }
        try {
            Object obj = reference.get();
            if (obj != null) {
                LOG.info("初始化引用成功{}", metaData);
                cache.put(metaData.getServiceName(), reference);
            }
        } catch (Exception ex) {
            LOG.error("初始化引用没有找到提供者【{}】,ex:{}", metaData, ex.getMessage());

        }
        return reference;
    }

    /**
     * Get reference config.
     *
     * @param <T>         the type parameter
     * @param serviceName the service name
     * @return the reference config
     */
    public <T> ReferenceConfig<T> get(final String serviceName) {
        try {
            return (ReferenceConfig<T>) cache.get(serviceName);
        } catch (ExecutionException e) {
            throw new SoulException(e.getCause());
        }
    }

    /**
     * Invalidate.
     *
     * @param serviceName the service name
     */
    public void invalidate(final String serviceName) {
        try {
            cache.invalidate(serviceName);
        } catch (Exception e) {
            throw new SoulException(e.getCause());
        }
    }

    /**
     * Invalidate all.
     */
    public void invalidateAll(){
        cache.invalidateAll();
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
