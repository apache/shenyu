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

package org.apache.shenyu.plugin.brpc.cache;

import com.baidu.cloud.starlight.api.rpc.StarlightClient;
import com.baidu.cloud.starlight.api.rpc.config.ServiceConfig;
import com.baidu.cloud.starlight.api.rpc.config.TransportConfig;
import com.baidu.cloud.starlight.core.rpc.SingleStarlightClient;
import com.baidu.cloud.starlight.core.rpc.generic.AsyncGenericService;
import com.baidu.cloud.starlight.core.rpc.proxy.JDKProxyFactory;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.cache.RemovalListener;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.convert.plugin.BrpcRegisterConfig;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.brpc.exception.ShenyuBrpcPluginException;
import org.apache.shenyu.plugin.brpc.util.ProxyInfoUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;

/**
 * Brpc config cache.
 */
public final class ApplicationConfigCache {

    /**
     * The constant PARAM_MAP.
     */
    public static final ConcurrentMap<String, BrpcParamInfo> PARAM_MAP = new ConcurrentHashMap<>();

    private static final String BRPC_PROTOCOL = "brpc";

    private static final Logger LOG = LoggerFactory.getLogger(ApplicationConfigCache.class);

    private static final ConcurrentMap<ServiceConfig, AsyncGenericService> PROXY_CACHE = new ConcurrentHashMap<>();
    
    private static final ConcurrentMap<ServiceConfig, StarlightClient> CLIENT_CACHE = new ConcurrentHashMap<>();

    private JDKProxyFactory proxyFactory;

    private final LoadingCache<String, ServiceConfig> cache = CacheBuilder.newBuilder()
            .maximumSize(Constants.CACHE_MAX_COUNT)
            .removalListener((RemovalListener<Object, ServiceConfig>) notification -> {
                ServiceConfig config = notification.getValue();
                if (Objects.nonNull(config)) {
                    // After the configuration change, destroys the instance, but does not empty it.
                    // If it is not handled, it will get NULL when reinitializing and cause a NULL pointer problem.
                    PROXY_CACHE.remove(config);
                    Optional.ofNullable(CLIENT_CACHE.get(config))
                            .ifPresent(StarlightClient::destroy);
                    CLIENT_CACHE.remove(config);
                }
            })
            .build(new CacheLoader<String, ServiceConfig>() {
                @Override
                public ServiceConfig load(@NonNull final String key) {
                    return new ServiceConfig();
                }
            });

    private ApplicationConfigCache() {
    }

    /**
     * build service.
     *
     * @param serviceConfig the service config
     * @param metaData the meta data
     * @return service
     */
    public AsyncGenericService buildService(final ServiceConfig serviceConfig,
                                            final MetaData metaData) {
        AsyncGenericService service = PROXY_CACHE.get(serviceConfig);
        if (Objects.nonNull(service)) {
            return service;
        }
        if (Objects.isNull(proxyFactory)) {
            proxyFactory = new JDKProxyFactory();
        }
        StarlightClient client = CLIENT_CACHE.get(serviceConfig);
        if (Objects.isNull(client)) {
            BrpcParamExtInfo brpcParamExtInfo = GsonUtils.getInstance().fromJson(metaData.getRpcExt(), BrpcParamExtInfo.class);
            TransportConfig transportConfig = new TransportConfig();
            transportConfig.setBizThreadPoolName(Constants.SHARED_BIZTHREADPOOLNAME);
            client = new SingleStarlightClient(brpcParamExtInfo.getHost(), brpcParamExtInfo.getPort(), transportConfig);
            client.init();
            CLIENT_CACHE.put(serviceConfig, client);
        }
        service = proxyFactory.getProxy(AsyncGenericService.class, serviceConfig, client);
        PROXY_CACHE.put(serviceConfig, service);
        return service;
    }
    
    /**
     * init service.
     *
     * @param metaData the meta data
     * @return service config
     */
    public ServiceConfig initRef(final MetaData metaData) {
        try {
            ServiceConfig serviceConfig = cache.get(metaData.getPath());
            if (StringUtils.isNotBlank(serviceConfig.getServiceId())) {
                return serviceConfig;
            }
        } catch (Exception e) {
            LOG.warn("init brpc ref ex:{}", e.getMessage());
        }
        return build(metaData);
    }

    /**
     * init brpc config.
     *
     * @param brpcRegisterConfig the config of brpc
     */
    public void init(final BrpcRegisterConfig brpcRegisterConfig) {
        if (Objects.isNull(proxyFactory)) {
            proxyFactory = new JDKProxyFactory();
        }
    }

    /**
     * Build service config.
     *
     * @param metaData the meta data
     * @return service config
     */
    public ServiceConfig build(final MetaData metaData) {
        ServiceConfig serviceConfig = new ServiceConfig();
        serviceConfig.setProtocol(BRPC_PROTOCOL);
        serviceConfig.setServiceId(metaData.getServiceName());

        BrpcParamExtInfo brpcParamExtInfo =
                GsonUtils.getInstance().fromJson(metaData.getRpcExt(), BrpcParamExtInfo.class);
        brpcParamExtInfo.getMethodInfo().forEach(methodInfo -> {
            if (CollectionUtils.isNotEmpty(methodInfo.getParamTypes())) {
                try {
                    Class<?>[] paramTypes = new Class[methodInfo.getParamTypes().size()];
                    String[] paramNames = new String[methodInfo.getParamTypes().size()];
                    for (int i = 0; i < methodInfo.getParamTypes().size(); i++) {
                        Pair<String, String> pair = methodInfo.getParamTypes().get(i);
                        paramTypes[i] = ProxyInfoUtil.getParamClass(pair.getKey());
                        paramNames[i] = pair.getValue();
                        PARAM_MAP.put(methodInfo.getMethodName(), new BrpcParamInfo(paramTypes, paramNames));
                    }
                } catch (Exception e) {
                    LOG.error("failed to init brpc, {}", e.getMessage());
                    throw new ShenyuBrpcPluginException(e.getCause());
                }
            }
        });
        return serviceConfig;
    }

    /**
     * Get service config.
     *
     * @param path path
     * @return the service config
     */
    public ServiceConfig get(final String path) {
        try {
            return cache.get(path);
        } catch (ExecutionException e) {
            throw new ShenyuBrpcPluginException(e.getCause());
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
     * Get param info key.
     *
     * @param className  className
     * @param methodName methodName
     * @return the key
     */
    public static String getClassMethodKey(final String className, final String methodName) {
        return String.join("_", className, methodName);
    }

    /**
     * Gets the proxy factory.
     *
     * @return the proxy factory
     */
    public JDKProxyFactory getProxyFactory() {
        return proxyFactory;
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
     * The type Brpc param ext info.
     */
    public static class MethodInfo {

        private String methodName;

        private List<Pair<String, String>> paramTypes;

        /**
         * Gets method name.
         *
         * @return the method name
         */
        public String getMethodName() {
            return methodName;
        }

        /**
         * Sets method name.
         *
         * @param methodName the method name
         */
        public void setMethodName(final String methodName) {
            this.methodName = methodName;
        }

        /**
         * Gets paramTypes.
         *
         * @return the paramTypes
         */
        public List<Pair<String, String>> getParamTypes() {
            return paramTypes;
        }

        /**
         * Sets paramTypes.
         *
         * @param paramTypes the paramTypes
         */
        public void setParamTypes(final List<Pair<String, String>> paramTypes) {
            this.paramTypes = paramTypes;
        }
    }

    /**
     * The type Brpc param ext info.
     */
    public static class BrpcParamExtInfo {

        private List<MethodInfo> methodInfo;
    
        private String host;
    
        private Integer port;

        /**
         * Gets method info.
         *
         * @return the method info
         */
        public List<MethodInfo> getMethodInfo() {
            return methodInfo;
        }

        /**
         * Sets method info.
         *
         * @param methodInfo the method info
         */
        public void setMethodInfo(final List<MethodInfo> methodInfo) {
            this.methodInfo = methodInfo;
        }
    
        /**
         * get port.
         *
         * @return port
         */
        public Integer getPort() {
            return port;
        }
    
        /**
         * set port.
         *
         * @param port port
         */
        public void setPort(final Integer port) {
            this.port = port;
        }
    
        /**
         * get host.
         *
         * @return host
         */
        public String getHost() {
            return host;
        }
    
        /**
         * set host.
         *
         * @param host host
         */
        public void setHost(final String host) {
            this.host = host;
        }
    }

    /**
     * The type Brpc param ext info.
     */
    public static class BrpcParamInfo {

        private Class<?>[] paramTypes;

        private String[] paramNames;

        BrpcParamInfo(final Class<?>[] paramTypes, final String[] paramNames) {
            this.paramTypes = paramTypes;
            this.paramNames = paramNames;
        }

        /**
         * Get param types class [ ].
         *
         * @return the class [ ]
         */
        public Class<?>[] getParamTypes() {
            return paramTypes;
        }

        /**
         * Sets param types.
         *
         * @param paramTypes the param types
         */
        public void setParamTypes(final Class<?>[] paramTypes) {
            this.paramTypes = paramTypes;
        }

        /**
         * Get param names string [ ].
         *
         * @return the string [ ]
         */
        public String[] getParamNames() {
            return paramNames;
        }

        /**
         * Sets param names.
         *
         * @param paramNames the param names
         */
        public void setParamNames(final String[] paramNames) {
            this.paramNames = paramNames;
        }
    }
}
