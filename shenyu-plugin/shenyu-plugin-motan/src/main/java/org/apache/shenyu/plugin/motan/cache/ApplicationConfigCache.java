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
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.MotanRegisterConfig;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.motan.util.PrxInfoUtil;

import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;

/**
 * The cache info.
 */
@Slf4j
public final class ApplicationConfigCache {

    public static final ConcurrentHashMap<String, MotanParamInfo> PARAM_MAP = new ConcurrentHashMap<>();

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
                        log.error("modify ref have exception", e);
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
     * @param <T>         the type parameter
     * @param path        path
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
            log.error("init motan ref ex:{}", e.getMessage());
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
                    log.error("failed to init motan, {}", e.getMessage());
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
            log.info("init motan reference success there meteData is :{}", metaData);
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
    @Data
    static class MethodInfo {

        private String methodName;

        private List<Pair<String, String>> params;
    }

    /**
     * The type Motan param ext info.
     */
    @Data
    static class MotanParamExtInfo {

        private List<MethodInfo> methodInfo;

        private String group;
    }

    /**
     * The type Motan param ext info.
     */
    @Data
    @AllArgsConstructor
    public static class MotanParamInfo {

        private Class<?>[] paramTypes;

        private String[] paramNames;
    }
}
