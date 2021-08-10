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

package org.apache.shenyu.plugin.sofa.cache;

import com.alipay.sofa.rpc.api.GenericService;
import com.alipay.sofa.rpc.common.RpcConstants;
import com.alipay.sofa.rpc.config.ApplicationConfig;
import com.alipay.sofa.rpc.config.ConsumerConfig;
import com.alipay.sofa.rpc.config.RegistryConfig;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.SofaRegisterConfig;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ExecutionException;

/**
 * The type Application config cache.
 */
public final class ApplicationConfigCache {

    private static final Logger LOG = LoggerFactory.getLogger(ApplicationConfigCache.class);

    private ApplicationConfig applicationConfig;

    private RegistryConfig registryConfig;

    private final int maxCount = 1000;

    private final LoadingCache<String, ConsumerConfig<GenericService>> cache = CacheBuilder.newBuilder()
            .maximumSize(maxCount)
            .removalListener(notification -> {
                ConsumerConfig<GenericService> config = (ConsumerConfig<GenericService>) notification.getValue();
                if (config != null) {
                    try {
                        Class<?> cz = config.getClass();
                        Field field = cz.getDeclaredField("consumerBootstrap");
                        field.setAccessible(true);
                        field.set(config, null);
                        // After the configuration change, sofa destroys the instance, but does not empty it. If it is not handled,
                        // it will get NULL when reinitializing and cause a NULL pointer problem.
                    } catch (NoSuchFieldException | IllegalAccessException e) {
                        LOG.error("modify ref have exception", e);
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
            applicationConfig.setAppId("shenyu_proxy");
            applicationConfig.setAppName("shenyu_proxy");
        }
        if (registryConfig == null) {
            registryConfig = new RegistryConfig();
            registryConfig.setProtocol(sofaRegisterConfig.getProtocol());
            registryConfig.setId("shenyu_proxy");
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
            ConsumerConfig<GenericService> referenceConfig = cache.get(metaData.getPath());
            if (StringUtils.isNoneBlank(referenceConfig.getInterfaceId())) {
                return referenceConfig;
            }
        } catch (ExecutionException e) {
            LOG.error("init sofa ref ex:{}", e.getMessage());
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
        reference.setProtocol(RpcConstants.PROTOCOL_TYPE_BOLT);
        reference.setInvokeType(RpcConstants.INVOKER_TYPE_CALLBACK);
        reference.setRepeatedReferLimit(-1);
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
            LOG.info("init sofa reference success there meteData is :{}", metaData);
            cache.put(metaData.getPath(), reference);
        }
        return reference;
    }

    private String buildLoadBalanceName(final String loadBalance) {
        if (LoadBalanceEnum.HASH.getName().equals(loadBalance) || StringUtils.equalsIgnoreCase("consistenthash", loadBalance)) {
            return "consistentHash";
        }
        if (LoadBalanceEnum.ROUND_ROBIN.getName().equals(loadBalance) || StringUtils.equalsIgnoreCase("roundrobin", loadBalance)) {
            return "roundRobin";
        }
        return loadBalance;
    }

    /**
     * Get reference config.
     *
     * @param <T>         the type parameter
     * @param path        path
     * @return the reference config
     */
    public <T> ConsumerConfig<T> get(final String path) {
        try {
            return (ConsumerConfig<T>) cache.get(path);
        } catch (ExecutionException e) {
            throw new ShenyuException(e.getCause());
        }
    }

    /**
     * Invalidate.
     *
     * @param path the path name
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
    static class SofaParamExtInfo {

        private String loadbalance;

        private Integer retries;

        private Integer timeout;

        public String getLoadbalance() {
            return loadbalance;
        }

        public void setLoadbalance(final String loadbalance) {
            this.loadbalance = loadbalance;
        }

        public Integer getRetries() {
            return retries;
        }

        public void setRetries(final Integer retries) {
            this.retries = retries;
        }

        public Integer getTimeout() {
            return timeout;
        }

        public void setTimeout(final Integer timeout) {
            this.timeout = timeout;
        }
    }
}
