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

package org.apache.shenyu.plugin.logging.common.handler;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.entity.CommonLoggingRuleHandle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/**
 * AbstractLogPluginDataHandler.
 */
public abstract class AbstractLogPluginDataHandler<T extends GenericGlobalConfig, C extends GenericApiConfig> implements PluginDataHandler {

    public static final Supplier<CommonHandleCache<String, CommonLoggingRuleHandle>> CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);

    protected static final Logger LOG = LoggerFactory.getLogger(AbstractLogPluginDataHandler.class);

    private static final String EMPTY_JSON = "{}";

    private static final Map<String, List<String>> SELECT_ID_URI_LIST_MAP = new ConcurrentHashMap<>();

    private static final Map<String, GenericApiConfig> SELECT_API_CONFIG_MAP = new ConcurrentHashMap<>();

    /**
     * get selectId uriList map.
     *
     * @return selectId uriList map
     */
    public static Map<String, List<String>> getSelectIdUriListMap() {
        return SELECT_ID_URI_LIST_MAP;
    }

    /**
     * get select api config map.
     *
     * @return select api config map
     */
    public static Map<String, GenericApiConfig> getSelectApiConfigMap() {
        return SELECT_API_CONFIG_MAP;
    }

    /**
     * LogCollector.
     *
     * @return LogCollector
     */
    protected abstract LogCollector logCollector();

    /**
     * LogCollector.
     *
     * @param globalLogConfig globalLogConfig
     */
    protected abstract void doRefreshConfig(T globalLogConfig);

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        ParameterizedType parameterizedType = (ParameterizedType) this.getClass().getGenericSuperclass();
        Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
        final Class<T> globalLogConfigClass = (Class<T>) actualTypeArguments[0];
        LOG.info("handler {} Plugin data: {}", pluginNamed(), GsonUtils.getGson().toJson(pluginData));
        if (Objects.nonNull(pluginData) && Boolean.TRUE.equals(pluginData.getEnabled())) {
            T globalLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), globalLogConfigClass);
            T exist = Singleton.INST.get(globalLogConfigClass);
            if (Objects.isNull(globalLogConfig)) {
                return;
            }
            if (Objects.isNull(exist) || !globalLogConfig.equals(exist)) {
                // no data, init client
                this.doRefreshConfig(globalLogConfig);
                logCollector().start();
            }
            Singleton.INST.single(globalLogConfigClass, globalLogConfig);
        } else {
            try {
                logCollector().close();
            } catch (Exception e) {
                LOG.error("{} close log collector error", this.getClass().getSimpleName(), e);
            }
        }
    }

    @Override
    public void handlerSelector(final SelectorData selectorData) {
        ParameterizedType parameterizedType = (ParameterizedType) this.getClass().getGenericSuperclass();
        Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
        final Class<C> genericApiConfigClass = (Class<C>) actualTypeArguments[1];
        LOG.info("handler {} selector data:{}", pluginNamed(), GsonUtils.getGson().toJson(selectorData));
        String handleJson = selectorData.getHandle();
        if (StringUtils.isEmpty(handleJson) || EMPTY_JSON.equals(handleJson.trim())) {
            return;
        }
        if (selectorData.getType() != SelectorTypeEnum.CUSTOM_FLOW.getCode()
                || CollectionUtils.isEmpty(selectorData.getConditionList())) {
            return;
        }
        GenericApiConfig logApiConfig = GsonUtils.getInstance().fromJson(handleJson, genericApiConfigClass);
        if (StringUtils.isBlank(logApiConfig.getTopic()) || StringUtils.isBlank(logApiConfig.getSampleRate())) {
            return;
        }
        List<String> uriList = new ArrayList<>();
        for (ConditionData conditionData : selectorData.getConditionList()) {
            if ("uri".equals(conditionData.getParamType()) && StringUtils.isNotBlank(conditionData.getParamValue())
                    && ("match".equals(conditionData.getOperator()) || "=".equals(conditionData.getOperator()))) {
                uriList.add(conditionData.getParamValue().trim());
            }
        }
        SELECT_ID_URI_LIST_MAP.put(selectorData.getId(), uriList);
        SELECT_API_CONFIG_MAP.put(selectorData.getId(), logApiConfig);
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        LOG.info("handler remove {} selector data:{}", pluginNamed(), GsonUtils.getGson().toJson(selectorData));
        SELECT_ID_URI_LIST_MAP.remove(selectorData.getId());
        SELECT_API_CONFIG_MAP.remove(selectorData.getId());
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            CommonLoggingRuleHandle commonLoggingRuleHandle = GsonUtils.getInstance().fromJson(s, CommonLoggingRuleHandle.class);
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), commonLoggingRuleHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }
}
