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
import com.weibo.api.motan.proxy.CommonClient;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.convert.plugin.MotanRegisterConfig;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ExecutionException;

/**
 * The cache info.
 */
public final class ApplicationConfigCache {

    private static final Logger LOG = LoggerFactory.getLogger(ApplicationConfigCache.class);

    private RegistryConfig registryConfig;

    private ProtocolConfig protocolConfig;

    private final LoadingCache<String, RefererConfig<CommonClient>> cache = CacheBuilder.newBuilder().maximumSize(Constants.CACHE_MAX_COUNT).removalListener(notification -> {
        RefererConfig<?> config = (RefererConfig<?>) notification.getValue();
        if (Objects.nonNull(config)) {
            try {
                Field field = FieldUtils.getDeclaredField(config.getClass(), "ref", true);
                field.set(config, null);
                // After the configuration change, motan destroys the instance, but does not empty it. If it is not handled,
                // it will get NULL when reinitializing and cause a NULL pointer problem.
            } catch (NullPointerException | IllegalAccessException e) {
                LOG.error("modify ref have exception", e);
            }
        }
    }).build(new CacheLoader<>() {
        @Override
        @NonNull
        public RefererConfig<CommonClient> load(@NonNull final String key) {
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
        if (Objects.isNull(registryConfig)) {
            registryConfig = new RegistryConfig();
            registryConfig.setId("shenyu_motan_proxy");
            registryConfig.setRegister(false);
            registryConfig.setRegProtocol(motanRegisterConfig.getRegisterProtocol());
            registryConfig.setAddress(motanRegisterConfig.getRegisterAddress());
        }
        if (Objects.isNull(protocolConfig)) {
            protocolConfig = new ProtocolConfig();
            protocolConfig.setId("motan2");
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
    @SuppressWarnings("unchecked")
    public <T> RefererConfig<T> get(final String path) {
        try {
            return (RefererConfig<T>) cache.get(path);
        } catch (ExecutionException e) {
            throw new ShenyuException(e);
        }
    }

    /**
     * Init ref reference config.
     *
     * @param metaData the meta data
     * @return the reference config
     */
    public RefererConfig<CommonClient> initRef(final MetaData metaData) {
        try {
            RefererConfig<CommonClient> referenceConfig = cache.get(metaData.getPath());
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
    public RefererConfig<CommonClient> build(final MetaData metaData) {
        if (Objects.isNull(protocolConfig) || Objects.isNull(registryConfig)) {
            return new RefererConfig<>();
        }
        RefererConfig<CommonClient> reference = new RefererConfig<>();
        reference.setInterface(CommonClient.class);
        reference.setServiceInterface(metaData.getServiceName());
        // the group of motan rpc call
        MotanParamExtInfo motanParamExtInfo = GsonUtils.getInstance().fromJson(metaData.getRpcExt(), MotanParamExtInfo.class);
        reference.setGroup(motanParamExtInfo.getGroup());
        reference.setVersion("1.0");
        reference.setRequestTimeout(Optional.ofNullable(motanParamExtInfo.getTimeout()).orElse(1000));
        reference.setRegistry(registryConfig);
        if (StringUtils.isNotEmpty(motanParamExtInfo.getRpcProtocol())) {
            protocolConfig.setName(motanParamExtInfo.getRpcProtocol());
            protocolConfig.setId(motanParamExtInfo.getRpcProtocol());
        }
        reference.setProtocol(protocolConfig);
        CommonClient obj = reference.getRef();
        if (Objects.nonNull(obj)) {
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
    static final class ApplicationConfigCacheInstance {

        /**
         * The Instance.
         */
        static final ApplicationConfigCache INSTANCE = new ApplicationConfigCache();

        private ApplicationConfigCacheInstance() {

        }
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

        private Integer timeout;

        private String rpcProtocol;

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

        public Integer getTimeout() {
            return timeout;
        }

        public void setTimeout(final Integer timeout) {
            this.timeout = timeout;
        }

        public String getRpcProtocol() {
            return rpcProtocol;
        }

        /**
         * Sets rpc protocol.
         *
         * @param rpcProtocol the rpc protocol
         */
        public void setRpcProtocol(final String rpcProtocol) {
            this.rpcProtocol = rpcProtocol;
        }
    }
}
