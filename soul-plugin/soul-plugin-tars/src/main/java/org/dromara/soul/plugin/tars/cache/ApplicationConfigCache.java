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
import com.qq.tars.client.Communicator;
import com.qq.tars.client.CommunicatorConfig;
import com.qq.tars.client.CommunicatorFactory;
import com.qq.tars.protocol.annotation.Servant;
import javafx.util.Pair;
import lombok.AllArgsConstructor;
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
import org.dromara.soul.plugin.tars.util.ReturnValueResolver;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Tars config cache.
 *
 * @author tydhot
 */
@Slf4j
public final class ApplicationConfigCache {

    private static final ReentrantLock LOCK = new ReentrantLock();

    private final int maxCount = 50000;

    private final LoadingCache<String, TarsInvokePrxList> cache = CacheBuilder.newBuilder()
            .maximumWeight(maxCount)
            .weigher((Weigher<String, TarsInvokePrxList>) (string, referenceConfig) -> getSize())
            .build(new CacheLoader<String, TarsInvokePrxList>() {
                @Override
                public TarsInvokePrxList load(final String key) {
                    return new TarsInvokePrxList(new CopyOnWriteArrayList<>(), null, null, null);
                }
            });

    private final ConcurrentHashMap<String, Class<?>> prxClassCache = new ConcurrentHashMap<>();

    private final ConcurrentHashMap<String, TarsParamInfo> prxParamCache = new ConcurrentHashMap<>();

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
     */
    @SuppressWarnings("all")
    public void initPrx(final MetaData metaData) {
        for (; ;) {
            Class<?> prxClass = prxClassCache.get(metaData.getServiceName());
            try {
                if (Objects.isNull(prxClass)) {
                    assert LOCK != null;
                    if (LOCK.tryLock()) {
                        try {
                            if (StringUtils.isEmpty(metaData.getRpcExt())) {
                                throw new SoulException("can't init prx with empty ext string");
                            }
                            String clazzName = PrxInfoUtil.getPrxName(metaData);
                            TarsParamExtInfo tarsParamExtInfo = GsonUtils.getInstance().fromJson(metaData.getRpcExt(), TarsParamExtInfo.class);
                            DynamicType.Builder<?> classDefinition = new ByteBuddy()
                                    .makeInterface()
                                    .name(clazzName);
                            for (MethodInfo methodInfo : tarsParamExtInfo.getMethodInfo()) {
                                DynamicType.Builder.MethodDefinition.ParameterDefinition<?> definition =
                                        classDefinition.defineMethod(PrxInfoUtil.getMethodName(methodInfo.methodName),
                                                ReturnValueResolver.getCallBackType(Class.forName(methodInfo.getReturnType())),
                                                Visibility.PUBLIC);
                                if (CollectionUtils.isNotEmpty(methodInfo.getParams())) {
                                    Class<?>[] paramTypes = new Class[methodInfo.getParams().size()];
                                    String[] paramNames = new String[methodInfo.getParams().size()];
                                    for (int i = 0; i < methodInfo.getParams().size(); i++) {
                                        Pair<String, String> pair = methodInfo.getParams().get(i);
                                        paramTypes[i] = PrxInfoUtil.getParamClass(pair.getKey());
                                        paramNames[i] = pair.getValue();
                                        definition = definition.withParameter(paramTypes[i], paramNames[i]);
                                        prxParamCache.put(getClassMethodKey(clazzName, methodInfo.getMethodName()), new TarsParamInfo(paramTypes, paramNames));
                                    }
                                    classDefinition = definition.withoutCode();
                                }
                            }
                            Class<?> prxClazz = classDefinition.annotateType(AnnotationDescription.Builder.ofType(Servant.class).build())
                                    .make()
                                    .load(Servant.class.getClassLoader(), ClassLoadingStrategy.Default.INJECTION)
                                    .getLoaded();
                            assert communicator != null;
                            prxClassCache.put(metaData.getServiceName(), prxClazz);
                        } finally {
                            LOCK.unlock();
                        }
                    }
                } else {
                    // if object name is same it will return same prx
                    Object prx = communicator.stringToProxy(prxClass, PrxInfoUtil.getObjectName(metaData));
                    TarsInvokePrxList tarsInvokePrxList = cache.get(metaData.getPath());
                    if (tarsInvokePrxList.getMethod() == null) {
                        TarsParamInfo tarsParamInfo = prxParamCache.get(getClassMethodKey(prxClass.getName(), metaData.getMethodName()));
                        Method method = prx.getClass().getDeclaredMethod(
                                PrxInfoUtil.getMethodName(metaData.getMethodName()), tarsParamInfo.getParamTypes());
                        tarsInvokePrxList.setMethod(method);
                        tarsInvokePrxList.setParamTypes(tarsParamInfo.getParamTypes());
                        tarsInvokePrxList.setParamNames(tarsParamInfo.getParamNames());
                    }
                    tarsInvokePrxList.getTarsInvokePrxList().add(new TarsInvokePrx(prx, metaData.getAppName()));
                    break;
                }
            } catch (Exception e) {
                log.error("init tars ref ex:{}", e.getMessage());
                break;
            }
        }
    }

    /**
     * Get param info key.
     *
     * @param className className
     * @param methodName methodName
     * @return the key
     */
    public static String getClassMethodKey(final String className, final String methodName) {
        return className + "_" + methodName;
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
    static class MethodInfo {
        
        private String methodName;

        private List<Pair<String, String>> params;

        private String returnType;
    }

    /**
     * The type Tars param ext info.
     */
    @Data
    static class TarsParamExtInfo {
        
        private List<MethodInfo> methodInfo;
    }

    /**
     * The type Tars param ext info.
     */
    @Data
    @AllArgsConstructor
    static class TarsParamInfo {
        
        private Class<?>[] paramTypes;

        private String[] paramNames;
    }
}
