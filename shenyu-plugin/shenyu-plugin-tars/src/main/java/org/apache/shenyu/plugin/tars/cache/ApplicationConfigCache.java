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

package org.apache.shenyu.plugin.tars.cache;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.qq.tars.client.Communicator;
import com.qq.tars.client.CommunicatorConfig;
import com.qq.tars.client.CommunicatorFactory;
import com.qq.tars.protocol.annotation.Servant;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.tars.proxy.TarsInvokePrx;
import org.apache.shenyu.plugin.tars.proxy.TarsInvokePrxList;
import org.apache.shenyu.plugin.tars.util.PrxInfoUtil;
import org.apache.shenyu.plugin.tars.util.ReturnValueResolver;
import org.assertj.core.internal.bytebuddy.ByteBuddy;
import org.assertj.core.internal.bytebuddy.description.annotation.AnnotationDescription;
import org.assertj.core.internal.bytebuddy.description.modifier.Visibility;
import org.assertj.core.internal.bytebuddy.dynamic.DynamicType;
import org.assertj.core.internal.bytebuddy.dynamic.loading.ClassLoadingStrategy;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

/**
 * Tars config cache.
 */
public final class ApplicationConfigCache {

    private static final Logger LOG = LoggerFactory.getLogger(ApplicationConfigCache.class);

    private static final ReentrantLock LOCK = new ReentrantLock();

    private final int maxCount = 1000;

    private final LoadingCache<String, TarsInvokePrxList> cache = CacheBuilder.newBuilder()
            .maximumSize(maxCount)
            .build(new CacheLoader<String, TarsInvokePrxList>() {
                @Override
                public TarsInvokePrxList load(final String key) {
                    return new TarsInvokePrxList(new CopyOnWriteArrayList<>(), null, null, null);
                }
            });

    private final ConcurrentHashMap<String, List<MetaData>> ctxPathCache = new ConcurrentHashMap<>();

    private final ConcurrentHashMap<String, Class<?>> prxClassCache = new ConcurrentHashMap<>();

    private final ConcurrentHashMap<String, TarsParamInfo> prxParamCache = new ConcurrentHashMap<>();

    private final ConcurrentHashMap<String, List<DivideUpstream>> refreshUpstreamCache = new ConcurrentHashMap<>();

    private final Communicator communicator;

    private ApplicationConfigCache() {
        communicator = CommunicatorFactory.getInstance().getCommunicator(CommunicatorConfig.getDefault());
    }

    /**
     * Get reference config.
     *
     * @param path path
     * @return the reference config
     */
    public TarsInvokePrxList get(final String path) {
        try {
            return cache.get(path);
        } catch (ExecutionException e) {
            throw new ShenyuException(e.getCause());
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
            Class<?> prxClass = prxClassCache.get(metaData.getPath());
            try {
                if (Objects.isNull(prxClass)) {
                    assert LOCK != null;
                    if (LOCK.tryLock()) {
                        try {
                            if (StringUtils.isEmpty(metaData.getRpcExt())) {
                                throw new ShenyuException("can't init prx with empty ext string");
                            }
                            String clazzName = PrxInfoUtil.getPrxName(metaData);
                            TarsParamExtInfo tarsParamExtInfo = GsonUtils.getInstance().fromJson(metaData.getRpcExt(), TarsParamExtInfo.class);
                            DynamicType.Builder<?> classDefinition = new ByteBuddy()
                                    .makeInterface()
                                    .name(clazzName);
                            for (MethodInfo methodInfo : tarsParamExtInfo.getMethodInfo()) {
                                DynamicType.Builder.MethodDefinition.ParameterDefinition<?> definition =
                                        classDefinition.defineMethod(PrxInfoUtil.getMethodName(methodInfo.methodName),
                                                ReturnValueResolver.getCallBackType(PrxInfoUtil.getParamClass(methodInfo.getReturnType())),
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
                            prxClassCache.put(metaData.getPath(), prxClazz);
                            List<MetaData> paths = ctxPathCache.getOrDefault(metaData.getContextPath(), new ArrayList<>());
                            if (!paths.contains(metaData.getPath())) {
                                paths.add(metaData);
                            }
                            ctxPathCache.put(metaData.getContextPath(), paths);
                        } finally {
                            LOCK.unlock();
                        }
                    }
                } else {
                    if (Objects.nonNull(metaData.getContextPath()) && Objects.nonNull(refreshUpstreamCache.get(metaData.getContextPath()))) {
                        refreshTarsInvokePrxList(metaData, refreshUpstreamCache.get(metaData.getContextPath()));
                    }
                    break;
                }
            } catch (Exception e) {
                LOG.error("init tars ref ex:{}", e.getMessage());
                break;
            }
        }
    }

    /**
     * Get param info key.
     *
     * @param className  className
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
     * initPrxClass.
     *
     * @param selectorData selectorData
     */
    public void initPrxClass(final SelectorData selectorData) {
        try {
            final List<DivideUpstream> upstreamList = GsonUtils.getInstance().fromList(selectorData.getHandle(), DivideUpstream.class);
            if (null == upstreamList || upstreamList.size() == 0) {
                invalidate(selectorData.getName());
                return;
            }
            refreshUpstreamCache.put(selectorData.getName(), upstreamList);
            List<MetaData> metaDataList = ctxPathCache.getOrDefault(selectorData.getName(), new ArrayList<>());
            for (MetaData metaData : metaDataList) {
                refreshTarsInvokePrxList(metaData, upstreamList);
            }
        } catch (ExecutionException | NoSuchMethodException e) {
            throw new ShenyuException(e.getCause());
        }
    }

    /**
     * refresh metaData path upstream url.
     *
     * @param metaData     metaData
     * @param upstreamList upstream list
     */
    private void refreshTarsInvokePrxList(final MetaData metaData, final List<DivideUpstream> upstreamList) throws NoSuchMethodException, ExecutionException {
        Class<?> prxClass = prxClassCache.get(metaData.getPath());
        if (Objects.isNull(prxClass)) {
            return;
        }
        TarsInvokePrxList tarsInvokePrxList = cache.get(metaData.getPath());
        tarsInvokePrxList.getTarsInvokePrxList().clear();
        if (tarsInvokePrxList.getMethod() == null) {
            TarsParamInfo tarsParamInfo = prxParamCache.get(getClassMethodKey(prxClass.getName(), metaData.getMethodName()));
            Object prx = communicator.stringToProxy(prxClass, PrxInfoUtil.getObjectName(upstreamList.get(0).getUpstreamUrl(), metaData.getServiceName()));
            Method method = prx.getClass().getDeclaredMethod(
                    PrxInfoUtil.getMethodName(metaData.getMethodName()), tarsParamInfo.getParamTypes());
            tarsInvokePrxList.setMethod(method);
            tarsInvokePrxList.setParamTypes(tarsParamInfo.getParamTypes());
            tarsInvokePrxList.setParamNames(tarsParamInfo.getParamNames());
        }
        tarsInvokePrxList.getTarsInvokePrxList().addAll(upstreamList.stream().map(upstream -> {
            Object strProxy = communicator.stringToProxy(prxClass, PrxInfoUtil.getObjectName(upstream.getUpstreamUrl(), metaData.getServiceName()));
            return new TarsInvokePrx(strProxy, upstream.getUpstreamUrl());
        }).collect(Collectors.toList()));
    }

    /**
     * invalidate.
     *
     * @param contextPath context path
     */
    public void invalidate(final String contextPath) {
        List<MetaData> metaDataList = ctxPathCache.getOrDefault(contextPath, new ArrayList<>());
        for (MetaData metaData : metaDataList) {
            cache.invalidate(metaData.getPath());
        }
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
    static class MethodInfo {

        private String methodName;

        private List<Pair<String, String>> params;

        private String returnType;

        public String getMethodName() {
            return methodName;
        }

        public void setMethodName(final String methodName) {
            this.methodName = methodName;
        }

        public List<Pair<String, String>> getParams() {
            return params;
        }

        public void setParams(final List<Pair<String, String>> params) {
            this.params = params;
        }

        public String getReturnType() {
            return returnType;
        }

        public void setReturnType(final String returnType) {
            this.returnType = returnType;
        }
    }

    /**
     * The type Tars param ext info.
     */
    static class TarsParamExtInfo {

        private List<MethodInfo> methodInfo;

        public List<MethodInfo> getMethodInfo() {
            return methodInfo;
        }

        public void setMethodInfo(final List<MethodInfo> methodInfo) {
            this.methodInfo = methodInfo;
        }
    }

    /**
     * The type Tars param ext info.
     */
    static class TarsParamInfo {

        private Class<?>[] paramTypes;

        private String[] paramNames;

        TarsParamInfo(final Class<?>[] paramTypes, final String[] paramNames) {
            this.paramTypes = paramTypes;
            this.paramNames = paramNames;
        }

        public Class<?>[] getParamTypes() {
            return paramTypes;
        }

        public void setParamTypes(final Class<?>[] paramTypes) {
            this.paramTypes = paramTypes;
        }

        public String[] getParamNames() {
            return paramNames;
        }

        public void setParamNames(final String[] paramNames) {
            this.paramNames = paramNames;
        }
    }
}
