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
import org.apache.commons.collections4.CollectionUtils;
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

    private StarlightClient clientConfig;

    private JDKProxyFactory proxyFactory;

    private final LoadingCache<String, ServiceConfig> serviceConfigCache = CacheBuilder.newBuilder()
            .maximumSize(Constants.CACHE_MAX_COUNT)
            .build(new CacheLoader<String, ServiceConfig>() {
                @Override
                public ServiceConfig load(@NonNull final String key) {
                    return null;
                }
            });

    private final LoadingCache<String, AsyncGenericService> serviceCache = CacheBuilder.newBuilder()
            .maximumSize(Constants.CACHE_MAX_COUNT)
            .build(new CacheLoader<String, AsyncGenericService>() {
                @Override
                public AsyncGenericService load(@NonNull final String key) {
                    return null;
                }
            });

    private ApplicationConfigCache() {
    }

    /**
     * init service.
     *
     * @param metaData the meta data
     * @return service
     */
    public AsyncGenericService initService(final MetaData metaData) {
        try {
            AsyncGenericService service = serviceCache.get(metaData.getPath());
            if (Objects.nonNull(service)) {
                return service;
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
        if (Objects.isNull(clientConfig)) {
            TransportConfig config = new TransportConfig();
            clientConfig = new SingleStarlightClient(brpcRegisterConfig.getAddress(), brpcRegisterConfig.getPort(), config);
            clientConfig.init();
            proxyFactory = new JDKProxyFactory();
        }
    }

    /**
     * Build config service.
     *
     * @param metaData the meta data
     * @return service config
     */
    public ServiceConfig buildServiceConfig(final MetaData metaData) {
        ServiceConfig serviceConfig = new ServiceConfig();
        serviceConfig.setProtocol(BRPC_PROTOCOL);
        serviceConfig.setServiceId(metaData.getServiceName());
        serviceConfigCache.put(metaData.getPath(), serviceConfig);
        return serviceConfig;
    }

    /**
     * Build service.
     *
     * @param metaData the meta data
     * @return service
     */
    public AsyncGenericService build(final MetaData metaData) {
        if (Objects.isNull(clientConfig)) {
            throw new UnsupportedOperationException("unsupport!!");
        }
        ServiceConfig serviceConfig = initServiceConfig(metaData);

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
        AsyncGenericService service = proxyFactory.getProxy(AsyncGenericService.class, serviceConfig, clientConfig);
        serviceCache.put(metaData.getPath(), service);
        return service;
    }

    /**
     * Get service.
     *
     * @param path path
     * @return the service
     */
    public AsyncGenericService get(final String path) {
        try {
            return serviceCache.get(path);
        } catch (ExecutionException e) {
            throw new ShenyuBrpcPluginException(e.getCause());
        }
    }

    /**
     * init brpc service config.
     *
     * @param metaData the meta data
     * @return service config
     */
    public ServiceConfig initServiceConfig(final MetaData metaData) {
        try {
            ServiceConfig config = serviceConfigCache.get(metaData.getPath());
            if (Objects.nonNull(config)) {
                return config;
            }
        } catch (Exception e) {
            LOG.warn("init brpc config ref ex:{}", e.getMessage());
        }
        return buildServiceConfig(metaData);
    }

    /**
     * Invalidate.
     *
     * @param path the path name
     */
    public void invalidate(final String path) {
        serviceConfigCache.invalidate(path);
    }

    /**
     * Invalidate all.
     */
    public void invalidateAll() {
        serviceConfigCache.invalidateAll();
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
     * Gets the client config.
     *
     * @return the client config
     */
    public StarlightClient getClientConfig() {
        return clientConfig;
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
