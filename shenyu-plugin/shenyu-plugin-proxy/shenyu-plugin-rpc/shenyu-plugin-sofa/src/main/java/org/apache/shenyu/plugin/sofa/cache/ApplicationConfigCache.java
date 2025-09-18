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
import com.google.common.collect.Maps;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.convert.plugin.SofaRegisterConfig;
import org.apache.shenyu.common.dto.convert.selector.SofaUpstream;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.DigestUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.lang.NonNull;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Field;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.StringJoiner;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * The type Application config cache.
 */
public final class ApplicationConfigCache {
    
    private static final Logger LOG = LoggerFactory.getLogger(ApplicationConfigCache.class);

    private static final Map<String, SofaUpstream> UPSTREAM_CACHE_MAP = Maps.newConcurrentMap();
    
    private final ThreadFactory factory = ShenyuThreadFactory.create("shenyu-sofa", true);
    
    private ApplicationConfig applicationConfig;
    
    private RegistryConfig registryConfig;
    
    private ThreadPoolExecutor threadPool;
    
    private final LoadingCache<String, ConsumerConfig<GenericService>> cache = CacheBuilder.newBuilder()
            .maximumSize(Constants.CACHE_MAX_COUNT)
            .removalListener(notification -> {
                if (Objects.nonNull(notification.getValue())) {
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
            .build(new CacheLoader<>() {
                
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
        if (Objects.isNull(applicationConfig)) {
            applicationConfig = new ApplicationConfig();
            applicationConfig.setAppId(shenyuProxy);
            applicationConfig.setAppName(shenyuProxy);
        }
        if (Objects.isNull(registryConfig)) {
            registryConfig = new RegistryConfig();
            registryConfig.setId(shenyuProxy);
            registryConfig.setRegister(false);

        }
        registryConfig.setProtocol(sofaRegisterConfig.getProtocol());
        registryConfig.setAddress(sofaRegisterConfig.getRegister());
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
     * Init ref reference config use sofaUpstream.
     *
     * @param selectorId    the selectorId
     * @param metaData      the meta data
     * @param sofaUpstream  the sofaUpstream
     * @return the reference config
     */
    public ConsumerConfig<GenericService> initRef(final String selectorId, final MetaData metaData, final SofaUpstream sofaUpstream) {
        try {
            String cacheKey = generateUpstreamCacheKey(selectorId, metaData.getPath(), sofaUpstream);
            ConsumerConfig<GenericService> referenceConfig = cache.get(cacheKey);
            if (StringUtils.isNoneBlank(referenceConfig.getInterfaceId())) {
                return referenceConfig;
            }
        } catch (ExecutionException e) {
            LOG.error("init sofa ref ex:{}", e.getMessage());
        }
        return build(metaData, selectorId, sofaUpstream);

    }

    /**
     * generate sofa upstream reference cache key.
     *
     * @param selectorId    selectorId
     * @param metaDataPath    metaDataPath
     * @param sofaUpstream  sofaUpstream
     * @return the reference config cache key
     */
    public String generateUpstreamCacheKey(final String selectorId, final String metaDataPath, final SofaUpstream sofaUpstream) {
        StringJoiner stringJoiner = new StringJoiner(Constants.SEPARATOR_UNDERLINE);
        stringJoiner.add(selectorId);
        stringJoiner.add(metaDataPath);
        if (StringUtils.isNotBlank(sofaUpstream.getProtocol())) {
            stringJoiner.add(sofaUpstream.getProtocol());
        }
        // use registry hash to short reference cache key
        String registryHash = DigestUtils.md5Hex(sofaUpstream.getRegister());
        stringJoiner.add(registryHash);
        return stringJoiner.toString();
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
        ConsumerConfig<GenericService> reference = buildReference(metaData);
        try {
            Object obj = reference.refer();
            if (Objects.nonNull(obj)) {
                LOG.info("init sofa reference success there meteData is :{}", metaData);
                cache.put(metaData.getPath(), reference);
            }
        } catch (Exception e) {
            LOG.error("init sofa reference exception", e);
        }
        return reference;
    }

    /**
     * build with dynamic namespace and selectorData's SofaUpstream、ruleData's custom param.
     *
     * @param metaData      metaData
     * @param selectorId    selectorId
     * @param sofaUpstream  sofaUpstream
     * @return the reference config
     */
    @SuppressWarnings("deprecation")
    public ConsumerConfig<GenericService> build(final MetaData metaData, final String selectorId, final SofaUpstream sofaUpstream) {
        if (Objects.isNull(sofaUpstream)) {
            return this.build(metaData);
        }

        ConsumerConfig<GenericService> reference = buildReference(metaData, sofaUpstream);
        try {
            Object obj = reference.refer();
            if (Objects.nonNull(obj)) {
                LOG.info("buildN init apache sofa reference success there meteData is :{}", metaData);
                String cacheKey = this.generateUpstreamCacheKey(selectorId, metaData.getPath(), sofaUpstream);
                cache.put(cacheKey, reference);
            }
        } catch (Exception e) {
            LOG.error("buildN init sofa reference exception", e);
        }
        return reference;
    }

    /**
     * buildReference param.
     *
     * @param metaData metaData
     * @return the reference config
     */
    private ConsumerConfig<GenericService> buildReference(final MetaData metaData) {
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
        return reference;
    }

    /**
     * buildReference param with sofaUpstream.
     *
     * @param metaData      metaData
     * @param sofaUpstream  sofaUpstream
     * @return the reference config
     */
    private ConsumerConfig<GenericService> buildReference(final MetaData metaData, final SofaUpstream sofaUpstream) {
        if (Objects.isNull(applicationConfig) || Objects.isNull(registryConfig)) {
            return new ConsumerConfig<>();
        }
        ConsumerConfig<GenericService> reference = new ConsumerConfig<>();
        reference.setGeneric(true);
        reference.setApplication(applicationConfig);

        if (Objects.nonNull(sofaUpstream)) {
            RegistryConfig registryConfigTemp = new RegistryConfig();
            registryConfigTemp.setProtocol(sofaUpstream.getProtocol());
            registryConfigTemp.setId(Constants.SOFA_DEFAULT_APPLICATION_NAME);
            registryConfigTemp.setRegister(false);
            registryConfigTemp.setAddress(sofaUpstream.getRegister());
            reference.setRegistry(registryConfigTemp);
        } else {
            reference.setRegistry(registryConfig);
        }

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
     * Get Upstream.
     *
     * @param path path
     * @return the upstream
     */
    public SofaUpstream getUpstream(final String path) {
        return UPSTREAM_CACHE_MAP.get(path);
    }

    /**
     * Put Upstream.
     *
     * @param path path
     * @param sofaUpstream sofaUpstream
     * @return the upstreamList
     */
    public SofaUpstream putUpstream(final String path, final SofaUpstream sofaUpstream) {
        return UPSTREAM_CACHE_MAP.put(path, sofaUpstream);
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
        UPSTREAM_CACHE_MAP.clear();
    }

    /**
     * Invalidate when sofa metadata update.
     *
     * @param metadataPath the metadataPath
     */
    public void invalidateWithMetadataPath(final String metadataPath) {
        ConcurrentMap<String, ConsumerConfig<GenericService>> map = cache.asMap();
        if (map.isEmpty()) {
            return;
        }
        Set<String> allKeys = map.keySet();
        Set<String> needInvalidateKeys = allKeys.stream().filter(key -> key.contains(metadataPath)).collect(Collectors.toSet());
        if (needInvalidateKeys.isEmpty()) {
            return;
        }
        needInvalidateKeys.forEach(cache::invalidate);
        needInvalidateKeys.forEach(UPSTREAM_CACHE_MAP::remove);
    }

    /**
     * Invalidate when sofa selector update.
     *
     * @param selectorId the selectorId
     */
    public void invalidateWithSelectorId(final String selectorId) {
        ConcurrentMap<String, ConsumerConfig<GenericService>> map = cache.asMap();
        if (map.isEmpty()) {
            return;
        }
        Set<String> allKeys = map.keySet();
        Set<String> needInvalidateKeys = allKeys.stream().filter(key -> key.contains(selectorId)).collect(Collectors.toSet());
        if (needInvalidateKeys.isEmpty()) {
            return;
        }
        needInvalidateKeys.forEach(cache::invalidate);
        needInvalidateKeys.forEach(UPSTREAM_CACHE_MAP::remove);
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
