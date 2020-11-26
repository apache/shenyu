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

package org.dromara.soul.plugin.sofa.cache;

import com.alipay.sofa.rpc.api.GenericService;
import com.alipay.sofa.rpc.config.ApplicationConfig;
import com.alipay.sofa.rpc.config.ConsumerConfig;
import com.alipay.sofa.rpc.config.RegistryConfig;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.cache.Weigher;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.config.SofaRegisterConfig;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.LoadBalanceEnum;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.GsonUtils;

import java.lang.reflect.Field;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ExecutionException;


/**
 * The type Application config cache.
 */
@Slf4j
public final class ApplicationConfigCache {
    
    private ApplicationConfig applicationConfig;
    
    private RegistryConfig registryConfig;
    
    private final int maxCount = 50000;
    
    private final LoadingCache<String, ConsumerConfig<GenericService>> cache = CacheBuilder.newBuilder()
            .maximumWeight(maxCount)
            .weigher((Weigher<String, ConsumerConfig<GenericService>>) (string, ReferenceConfig) -> getSize())
            .removalListener(notification -> {
                ConsumerConfig<GenericService> config = notification.getValue();
                if (config != null) {
                    try {
                        Class<?> cz = config.getClass();
                        Field field = cz.getDeclaredField("consumerBootstrap");
                        field.setAccessible(true);
                        field.set(config, null);
                        //跟改配置之后sofa 销毁该实例,但是未置空,如果不处理,重新初始化的时候将获取到NULL照成空指针问题.
                    } catch (NoSuchFieldException | IllegalAccessException e) {
                        log.error("modify ref have exception", e);
                    }
                }
            })
            .build(new CacheLoader<String, ConsumerConfig<GenericService>>() {
                @Override
                public ConsumerConfig<GenericService> load(final String key) {
                    return new ConsumerConfig<>();
                }
            });
    
    private ApplicationConfigCache() {
    }
    
    private int getSize() {
        return (int) cache.size();
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
     * Init.
     *
     * @param sofaRegisterConfig the sofa register config
     */
    public void init(final SofaRegisterConfig sofaRegisterConfig) {
        if (applicationConfig == null) {
            applicationConfig = new ApplicationConfig();
            applicationConfig.setAppId("soul_proxy");
            applicationConfig.setAppName("soul_proxy");
        }
        if (registryConfig == null) {
            registryConfig = new RegistryConfig();
            registryConfig.setProtocol(sofaRegisterConfig.getProtocol());
            registryConfig.setId("soul_proxy");
            registryConfig.setRegister(false);
            registryConfig.setAddress(sofaRegisterConfig.getRegister());
        }
    }
    
    /**
     * Init ref reference config.
     *
     * @param metaData the meta data
     * @return the reference config
     */
    public ConsumerConfig<GenericService> initRef(final MetaData metaData) {
        try {
            ConsumerConfig<GenericService> referenceConfig = cache.get(metaData.getServiceName());
            if (StringUtils.isNoneBlank(referenceConfig.getInterfaceId())) {
                return referenceConfig;
            }
        } catch (ExecutionException e) {
            log.error("init sofa ref ex:{}", e.getMessage());
        }
        return build(metaData);
        
    }
    
    /**
     * Build reference config.
     *
     * @param metaData the meta data
     * @return the reference config
     */
    public ConsumerConfig<GenericService> build(final MetaData metaData) {
        ConsumerConfig<GenericService> reference = new ConsumerConfig<>();
        reference.setGeneric(true);
        reference.setApplication(applicationConfig);
        reference.setRegistry(registryConfig);
        reference.setInterfaceId(metaData.getServiceName());
        reference.setProtocol("bolt");
        reference.setInvokeType("callback");
        String rpcExt = metaData.getRpcExt();
        SofaParamExtInfo sofaParamExtInfo = GsonUtils.getInstance().fromJson(rpcExt, SofaParamExtInfo.class);
        if (Objects.nonNull(sofaParamExtInfo)) {
            if (StringUtils.isNoneBlank(sofaParamExtInfo.getLoadbalance())) {
                final String loadBalance = sofaParamExtInfo.getLoadbalance();
                reference.setLoadBalancer(buildLoadBalanceName(loadBalance));
            }
            Optional.ofNullable(sofaParamExtInfo.getTimeout()).ifPresent(reference::setTimeout);
            Optional.ofNullable(sofaParamExtInfo.getRetries()).ifPresent(reference::setRetries);
        }
        Object obj = reference.refer();
        if (obj != null) {
            log.info("init sofa reference success there meteData is :{}", metaData.toString());
            cache.put(metaData.getServiceName(), reference);
        }
        return reference;
    }
    
    private String buildLoadBalanceName(final String loadBalance) {
        if (LoadBalanceEnum.HASH.getName().equals(loadBalance) || "consistenthash".equals(loadBalance)) {
            return "consistenthash";
        } else if (LoadBalanceEnum.ROUND_ROBIN.getName().equals(loadBalance)) {
            return "roundrobin";
        } else {
            return loadBalance;
        }
    }
    
    /**
     * Get reference config.
     *
     * @param <T>         the type parameter
     * @param serviceName the service name
     * @return the reference config
     */
    public <T> ConsumerConfig<T> get(final String serviceName) {
        try {
            return (ConsumerConfig<T>) cache.get(serviceName);
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
        cache.invalidate(serviceName);
    }
    
    /**
     * Invalidate all.
     */
    public void invalidateAll() {
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
    
    /**
     * The type Sofa param ext info.
     */
    @Data
    static class SofaParamExtInfo {
        
        private String loadbalance;
        
        private Integer retries;
        
        private Integer timeout;
    }
}
