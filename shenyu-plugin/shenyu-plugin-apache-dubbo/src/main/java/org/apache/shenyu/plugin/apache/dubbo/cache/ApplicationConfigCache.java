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

package org.apache.shenyu.plugin.apache.dubbo.cache;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import org.apache.commons.lang3.StringUtils;
import org.apache.dubbo.config.ApplicationConfig;
import org.apache.dubbo.config.ReferenceConfig;
import org.apache.dubbo.config.RegistryConfig;
import org.apache.dubbo.rpc.service.GenericService;
import org.apache.shenyu.common.config.DubboRegisterConfig;
import org.apache.shenyu.common.dto.MetaData;
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

    private final LoadingCache<String, ReferenceConfig<GenericService>> cache = CacheBuilder.newBuilder()
            .maximumSize(maxCount)
            .removalListener(notification -> {
                ReferenceConfig<GenericService> config = (ReferenceConfig<GenericService>) notification.getValue();
                if (config != null) {
                    try {
                        Class<?> cz = config.getClass();
                        Field field = cz.getDeclaredField("ref");
                        field.setAccessible(true);
                        // After the configuration change, Dubbo destroys the instance, but does not empty it. If it is not handled,
                        // it will get NULL when reinitializing and cause a NULL pointer problem.
                        field.set(config, null);
                    } catch (NoSuchFieldException | IllegalAccessException e) {
                        LOG.error("modify ref have exception", e);
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
     * @param dubboRegisterConfig the dubbo register config
     */
    public void init(final DubboRegisterConfig dubboRegisterConfig) {
        if (applicationConfig == null) {
            applicationConfig = new ApplicationConfig("shenyu_proxy");
        }
        if (needUpdateRegistryConfig(dubboRegisterConfig)) {
            RegistryConfig registryConfigTemp = new RegistryConfig();
            registryConfigTemp.setProtocol(dubboRegisterConfig.getProtocol());
            registryConfigTemp.setId("shenyu_proxy");
            registryConfigTemp.setRegister(false);
            registryConfigTemp.setAddress(dubboRegisterConfig.getRegister());
            Optional.ofNullable(dubboRegisterConfig.getGroup()).ifPresent(registryConfigTemp::setGroup);
            registryConfig = registryConfigTemp;
        }
    }

    private boolean needUpdateRegistryConfig(final DubboRegisterConfig dubboRegisterConfig) {
        if (registryConfig == null) {
            return true;
        }
        if (!Objects.equals(dubboRegisterConfig.getProtocol(), registryConfig.getProtocol())
                || !Objects.equals(dubboRegisterConfig.getRegister(), registryConfig.getAddress())
                || !Objects.equals(dubboRegisterConfig.getProtocol(), registryConfig.getProtocol())) {
            return true;
        }
        return false;
    }

    /**
     * Init ref reference config.
     *
     * @param metaData the meta data
     * @return the reference config
     */
    public ReferenceConfig<GenericService> initRef(final MetaData metaData) {
        try {
            ReferenceConfig<GenericService> referenceConfig = cache.get(metaData.getPath());
            if (StringUtils.isNoneBlank(referenceConfig.getInterface())) {
                return referenceConfig;
            }
        } catch (ExecutionException e) {
            LOG.error("init dubbo ref exception", e);
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
        if (Objects.isNull(applicationConfig) || Objects.isNull(registryConfig)) {
            return new ReferenceConfig<>();
        }
        ReferenceConfig<GenericService> reference = new ReferenceConfig<>();
        reference.setGeneric("true");
        reference.setAsync(true);
        reference.setApplication(applicationConfig);
        reference.setRegistry(registryConfig);
        reference.setInterface(metaData.getServiceName());
        reference.setProtocol("dubbo");
        String rpcExt = metaData.getRpcExt();
        DubboParamExtInfo dubboParamExtInfo = GsonUtils.getInstance().fromJson(rpcExt, DubboParamExtInfo.class);
        if (Objects.nonNull(dubboParamExtInfo)) {
            if (StringUtils.isNoneBlank(dubboParamExtInfo.getVersion())) {
                reference.setVersion(dubboParamExtInfo.getVersion());
            }
            if (StringUtils.isNoneBlank(dubboParamExtInfo.getGroup())) {
                reference.setGroup(dubboParamExtInfo.getGroup());
            }
            if (StringUtils.isNoneBlank(dubboParamExtInfo.getLoadbalance())) {
                reference.setLoadbalance(dubboParamExtInfo.getLoadbalance());
            }
            if (StringUtils.isNoneBlank(dubboParamExtInfo.getUrl())) {
                reference.setUrl(dubboParamExtInfo.getUrl());
            }
            Optional.ofNullable(dubboParamExtInfo.getTimeout()).ifPresent(reference::setTimeout);
            Optional.ofNullable(dubboParamExtInfo.getRetries()).ifPresent(reference::setRetries);
        }
        try {
            Object obj = reference.get();
            if (obj != null) {
                LOG.info("init apache dubbo reference success there meteData is :{}", metaData);
                cache.put(metaData.getPath(), reference);
            }
        } catch (Exception e) {
            LOG.error("init apache dubbo reference exception", e);
        }
        return reference;
    }

    /**
     * Get reference config.
     *
     * @param <T>  the type parameter
     * @param path the path
     * @return the reference config
     */
    public <T> ReferenceConfig<T> get(final String path) {
        try {
            return (ReferenceConfig<T>) cache.get(path);
        } catch (ExecutionException e) {
            throw new ShenyuException(e.getCause());
        }
    }

    /**
     * Invalidate.
     *
     * @param path the path
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
     * The type Dubbo param ext info.
     */
    static class DubboParamExtInfo {

        private String group;

        private String version;

        private String loadbalance;

        private Integer retries;

        private Integer timeout;

        private String url;

        public String getGroup() {
            return group;
        }

        public void setGroup(final String group) {
            this.group = group;
        }

        public String getVersion() {
            return version;
        }

        public void setVersion(final String version) {
            this.version = version;
        }

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

        public String getUrl() {
            return url;
        }

        public void setUrl(final String url) {
            this.url = url;
        }
    }
}
