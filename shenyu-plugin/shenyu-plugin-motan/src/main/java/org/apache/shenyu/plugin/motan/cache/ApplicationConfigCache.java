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

package org.apache.shenyu.plugin.motan.cache;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.weibo.api.motan.config.ProtocolConfig;
import com.weibo.api.motan.config.RefererConfig;
import com.weibo.api.motan.config.RegistryConfig;
import com.weibo.api.motan.proxy.CommonHandler;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.MotanRegisterConfig;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.motan.util.PrxInfoUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;

/**
 * The cache info.
 */
public final class ApplicationConfigCache {

    /**
     * The constant PARAM_MAP.
     */
    public static final ConcurrentHashMap<String, MotanParamInfo> PARAM_MAP = new ConcurrentHashMap<>();

    private static final Logger LOG = LoggerFactory.getLogger(ApplicationConfigCache.class);

    private RegistryConfig registryConfig;

    private ProtocolConfig protocolConfig;

    private final int maxCount = 1000;

    private final LoadingCache<String, RefererConfig<CommonHandler>> cache = CacheBuilder.newBuilder()
            .maximumSize(maxCount)
            .removalListener(notification -> {
                RefererConfig<CommonHandler> config = (RefererConfig<CommonHandler>) notification.getValue();
                if (config != null) {
                    try {
                        Class<?> cz = config.getClass();
                        Field field = cz.getDeclaredField("ref");
                        field.setAccessible(true);
                        field.set(config, null);
                        // After the configuration change, motan destroys the instance, but does not empty it. If it is not handled,
                        // it will get NULL when reinitializing and cause a NULL pointer problem.
                    } catch (NoSuchFieldException | IllegalAccessException e) {
                        LOG.error("modify ref have exception", e);
                    }
                }
            })
            .build(new CacheLoader<String, RefererConfig<CommonHandler>>() {
                @Override
                public RefererConfig<CommonHandler> load(final String key) {
                    return new RefererConfig<>();
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
     * @param motanRegisterConfig the motan register config
     */
    public void init(final MotanRegisterConfig motanRegisterConfig) {
        if (registryConfig == null) {
            registryConfig = new RegistryConfig();
            registryConfig.setId("shenyu_motan_proxy");
            registryConfig.setRegister(false);
            registryConfig.setRegProtocol("zookeeper");
            registryConfig.setAddress(motanRegisterConfig.getRegister());
        }
        if (protocolConfig == null) {
            protocolConfig = new ProtocolConfig();
            protocolConfig.setId("motan2-breeze");
            protocolConfig.setName("motan2");
        }
    }

    /**
     * Get reference config.
     *
     * @param <T>  the type parameter
     * @param path path
     * @return the reference config
     */
    public <T> RefererConfig<T> get(final String path) {
        try {
            return (RefererConfig<T>) cache.get(path);
        } catch (ExecutionException e) {
            throw new ShenyuException(e.getCause());
        }
    }

    /**
     * Init ref reference config.
     *
     * @param metaData the meta data
     * @return the reference config
     */
    public RefererConfig<CommonHandler> initRef(final MetaData metaData) {
        try {
            RefererConfig<CommonHandler> referenceConfig = cache.get(metaData.getPath());
            if (StringUtils.isNoneBlank(referenceConfig.getServiceInterface())) {
                return referenceConfig;
            }
        } catch (ExecutionException e) {
            LOG.error("init motan ref ex:{}", e.getMessage());
        }
        return build(metaData);

    }

    /**
     * Build reference config.
     *
     * @param metaData the meta data
     * @return the reference config
     */
    public RefererConfig<CommonHandler> build(final MetaData metaData) {
        if (Objects.isNull(protocolConfig) || Objects.isNull(registryConfig)) {
            return new RefererConfig<>();
        }
        RefererConfig<CommonHandler> reference = new RefererConfig<>();
        reference.setInterface(CommonHandler.class);
        reference.setServiceInterface(metaData.getServiceName());
        // the group of motan rpc call
        MotanParamExtInfo motanParamExtInfo =
                GsonUtils.getInstance().fromJson(metaData.getRpcExt(), MotanParamExtInfo.class);
        for (MethodInfo methodInfo : motanParamExtInfo.getMethodInfo()) {
            if (CollectionUtils.isNotEmpty(methodInfo.getParams())) {
                try {
                    Class<?>[] paramTypes = new Class[methodInfo.getParams().size()];
                    String[] paramNames = new String[methodInfo.getParams().size()];
                    for (int i = 0; i < methodInfo.getParams().size(); i++) {
                        Pair<String, String> pair = methodInfo.getParams().get(i);
                        paramTypes[i] = PrxInfoUtil.getParamClass(pair.getKey());
                        paramNames[i] = pair.getValue();
                        PARAM_MAP.put(methodInfo.getMethodName(), new MotanParamInfo(paramTypes, paramNames));
                    }
                } catch (Exception e) {
                    LOG.error("failed to init motan, {}", e.getMessage());
                }
            }
        }
        reference.setGroup(motanParamExtInfo.getGroup());
        reference.setVersion("1.0");
        reference.setRequestTimeout(1000);
        reference.setRegistry(registryConfig);
        reference.setProtocol(protocolConfig);
        CommonHandler obj = reference.getRef();
        if (obj != null) {
            LOG.info("init motan reference success there meteData is :{}", metaData);
            cache.put(metaData.getPath(), reference);
        }
        return reference;
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
     * The type Motan param ext info.
     */
    static class MethodInfo {

        private String methodName;

        private List<Pair<String, String>> params;

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
         * Gets params.
         *
         * @return the params
         */
        public List<Pair<String, String>> getParams() {
            return params;
        }

        /**
         * Sets params.
         *
         * @param params the params
         */
        public void setParams(final List<Pair<String, String>> params) {
            this.params = params;
        }
    }

    /**
     * The type Motan param ext info.
     */
    static class MotanParamExtInfo {

        private List<MethodInfo> methodInfo;

        private String group;

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
         * Gets group.
         *
         * @return the group
         */
        public String getGroup() {
            return group;
        }

        /**
         * Sets group.
         *
         * @param group the group
         */
        public void setGroup(final String group) {
            this.group = group;
        }
    }

    /**
     * The type Motan param ext info.
     */
    public static class MotanParamInfo {

        private Class<?>[] paramTypes;

        private String[] paramNames;

        /**
         * Instantiates a new Motan param info.
         *
         * @param paramTypes the param types
         * @param paramNames the param names
         */
        public MotanParamInfo(final Class<?>[] paramTypes, final String[] paramNames) {
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
