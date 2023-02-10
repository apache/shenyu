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
import com.alipay.sofa.rpc.context.AsyncRuntime;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.convert.plugin.SofaRegisterConfig;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.lang.NonNull;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Field;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * The type Application config cache.
 */
public final class ApplicationConfigCache {
    
    private static final Logger LOG = LoggerFactory.getLogger(ApplicationConfigCache.class);
    
    private final ThreadFactory factory = ShenyuThreadFactory.create("shenyu-sofa", true);
    
    private ApplicationConfig applicationConfig;
    
    private RegistryConfig registryConfig;
    
    private ThreadPoolExecutor threadPool;
    
    private final LoadingCache<String, ConsumerConfig<GenericService>> cache = CacheBuilder.newBuilder()
            .maximumSize(Constants.CACHE_MAX_COUNT)
            .removalListener(notification -> {
                if (notification.getValue() != null) {
                    try {
                        Class<?> cz = notification.getValue().getClass();
                        final Field field = FieldUtils.getDeclaredField(cz, "consumerBootstrap", true);
                        FieldUtils.writeField(field, notification.getValue(), null);
                        // After the configuration change, sofa destroys the instance, but does not empty it. If it is not handled,
                        // it will get NULL when reinitializing and cause a NULL pointer problem.
                    } catch (IllegalAccessException e) {
                        LOG.error("modify ref have exception", e);
                    }
                }
            })
            .build(new CacheLoader<String, ConsumerConfig<GenericService>>() {
                
                @Override
                @NonNull
                public ConsumerConfig<GenericService> load(@NonNull final String key) {
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
        final String shenyuProxy = "shenyu_proxy";
        if (applicationConfig == null) {
            applicationConfig = new ApplicationConfig();
            applicationConfig.setAppId(shenyuProxy);
            applicationConfig.setAppName(shenyuProxy);
        }
        if (registryConfig == null) {
            registryConfig = new RegistryConfig();
            registryConfig.setProtocol(sofaRegisterConfig.getProtocol());
            registryConfig.setId(shenyuProxy);
            registryConfig.setRegister(false);
            registryConfig.setAddress(sofaRegisterConfig.getRegister());
        }
        if (StringUtils.isNotBlank(sofaRegisterConfig.getThreadpool())) {
            initThreadPool(sofaRegisterConfig);
            Optional.ofNullable(threadPool).ifPresent(this::setAsyncRuntimeThreadPool);
        }
    }
    
    /**
     * Set sofa asyncRuntime thread pool.
     */
    private void setAsyncRuntimeThreadPool(final ThreadPoolExecutor threadPool) {
        Field field = ReflectionUtils.findField(AsyncRuntime.class, "asyncThreadPool");
        ReflectionUtils.makeAccessible(field);
        ReflectionUtils.setField(field, AsyncRuntime.class, threadPool);
    }
    
    /**
     * Init thread pool.
     */
    private void initThreadPool(final SofaRegisterConfig config) {
        if (Objects.nonNull(threadPool)) {
            return;
        }
        switch (config.getThreadpool()) {
            case Constants.SHARED:
                try {
                    threadPool = SpringBeanUtils.getInstance().getBean(ShenyuThreadPoolExecutor.class);
                    return;
                } catch (NoSuchBeanDefinitionException t) {
                    throw new ShenyuException("shared thread pool is not enable, config ${shenyu.sharedPool.enable} in your xml/yml !", t);
                }
            case Constants.FIXED:
            case Constants.EAGER:
            case Constants.LIMITED:
                throw new UnsupportedOperationException();
            case Constants.CACHED:
                int corePoolSize = Optional.ofNullable(config.getCorethreads()).orElse(0);
                int maximumPoolSize = Optional.ofNullable(config.getThreads()).orElse(Integer.MAX_VALUE);
                int queueSize = Optional.ofNullable(config.getQueues()).orElse(0);
                threadPool = new ThreadPoolExecutor(corePoolSize, maximumPoolSize, 60L, TimeUnit.SECONDS,
                        queueSize > 0 ? new LinkedBlockingQueue<>(queueSize) : new SynchronousQueue<>(), factory);
                return;
            default:
                return;
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
        if (Objects.isNull(applicationConfig) || Objects.isNull(registryConfig)) {
            return new ConsumerConfig<>();
        }
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
        try {
            Object obj = reference.refer();
            if (obj != null) {
                LOG.info("init sofa reference success there meteData is :{}", metaData);
                cache.put(metaData.getPath(), reference);
            }
        } catch (Exception e) {
            LOG.error("init sofa reference exception", e);
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
     * @param path path
     * @return the reference config
     */
    public ConsumerConfig<GenericService> get(final String path) {
        try {
            return cache.get(path);
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
     * get thread pool, just for integrated test.
     *
     * @return the thread pool
     */
    public ThreadPoolExecutor getThreadPool() {
        return threadPool;
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
