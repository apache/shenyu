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

package org.dromara.soul.plugin.tars.cache;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.cache.Weigher;
import com.google.common.reflect.TypeToken;
import com.qq.tars.client.Communicator;
import com.qq.tars.client.CommunicatorConfig;
import com.qq.tars.client.CommunicatorFactory;
import com.qq.tars.protocol.annotation.Servant;
import javafx.util.Pair;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.assertj.core.internal.bytebuddy.ByteBuddy;
import org.assertj.core.internal.bytebuddy.description.annotation.AnnotationDescription;
import org.assertj.core.internal.bytebuddy.description.modifier.Visibility;
import org.assertj.core.internal.bytebuddy.dynamic.DynamicType;
import org.assertj.core.internal.bytebuddy.dynamic.loading.ClassLoadingStrategy;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.tars.proxy.TarsInvokePrx;
import org.dromara.soul.plugin.tars.proxy.TarsInvokePrxList;
import org.dromara.soul.plugin.tars.util.PrxInfoUtil;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;

/**
 * Tars config cache.
 *
 * @author tydhot
 */
@Slf4j
public final class ApplicationConfigCache {

    private final int maxCount = 50000;

    private final LoadingCache<String, TarsInvokePrxList> cache = CacheBuilder.newBuilder()
            .maximumWeight(maxCount)
            .weigher((Weigher<String, TarsInvokePrxList>) (string, referenceConfig) -> getSize())
            .build(new CacheLoader<String, TarsInvokePrxList>() {
                @Override
                public TarsInvokePrxList load(final String key) {
                    return new TarsInvokePrxList(new CopyOnWriteArrayList<>(), null, null);
                }
            });

    private final ConcurrentHashMap<String, Class> prxClassCache = new ConcurrentHashMap<>();

    private final Communicator communicator;

    private ApplicationConfigCache() {
        communicator = CommunicatorFactory.getInstance().getCommunicator(CommunicatorConfig.getDefault());
    }

    private int getSize() {
        return (int) cache.size();
    }

    /**
     * Get reference config.
     *
     * @param path        path
     * @return the reference config
     */
    public TarsInvokePrxList get(final String path) {
        try {
            return cache.get(path);
        } catch (ExecutionException e) {
            throw new SoulException(e.getCause());
        }
    }

    /**
     * Init prx.
     *
     * @param metaData metaData
     * @return the prx
     */
    public TarsInvokePrx initPrx(final MetaData metaData) {
        Class prxClass = prxClassCache.get(metaData.getPath());
        try {
            if (Objects.isNull(prxClass)) {
                if (StringUtils.isEmpty(metaData.getRpcExt())) {
                    throw new SoulException("can't init prx with empty ext stirng");
                }
                TarsParamExtInfo tarsParamExtInfo = GsonUtils.getInstance().fromJson(metaData.getRpcExt(), TarsParamExtInfo.class);
                DynamicType.Builder.MethodDefinition.ParameterDefinition definition = new ByteBuddy()
                        .makeInterface()
                        .name(PrxInfoUtil.gerPrxName(metaData))
                        .defineMethod(PrxInfoUtil.gerMethodName(metaData), new TypeToken<CompletableFuture<String>>() { } .getType(), Visibility.PUBLIC);
                if (CollectionUtils.isNotEmpty(tarsParamExtInfo.getParams())) {
                    Class[] paramTypes = new Class[tarsParamExtInfo.getParams().size()];
                    String[] paramNames = new String[tarsParamExtInfo.getParams().size()];
                    for (int i = 0; i < tarsParamExtInfo.getParams().size(); i++) {
                        Pair<String, String> pair = tarsParamExtInfo.getParams().get(i);
                        paramTypes[i] = PrxInfoUtil.getParamClass(pair.getKey());
                        paramNames[i] = pair.getValue();
                        definition = definition.withParameter(paramTypes[i], paramNames[i]);
                    }
                    cache.get(metaData.getPath()).setParamTypes(paramTypes);
                    cache.get(metaData.getPath()).setParamNames(paramNames);
                }
                prxClass = definition.withoutCode()
                        .annotateType(AnnotationDescription.Builder.ofType(Servant.class).build())
                        .make()
                        .load(Servant.class.getClassLoader(), ClassLoadingStrategy.Default.INJECTION)
                        .getLoaded();
                prxClassCache.put(metaData.getPath(), prxClass);
            }
            Object prx = communicator.stringToProxy(prxClass, PrxInfoUtil.gerObjectName(metaData));
            TarsInvokePrx invoker = TarsInvokePrx.builder().host(metaData.getAppName()).invokePrx(prx).build();
            cache.get(metaData.getPath()).getTarsInvokePrxList().add(new TarsInvokePrx(prx, metaData.getServiceName()));
            return invoker;
        } catch (Exception e) {
            log.error("init tars ref ex:{}", e.getMessage());
        }
        return null;
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
    static class ApplicationConfigCacheInstance {
        /**
         * The Instance.
         */
        static final ApplicationConfigCache INSTANCE = new ApplicationConfigCache();
    }

    /**
     * The type Tars param ext info.
     */
    @Data
    static class TarsParamExtInfo {
        private String returnType;

        private List<Pair<String, String>> params;
    }
}
