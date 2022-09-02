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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * AbstractPluginDataHandler.
 */
public abstract class AbstractPluginDataHandler<C extends GenericApiConfig> implements PluginDataHandler {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractPluginDataHandler.class);

    private static final String EMPTY_JSON = "{}";

    private static final Map<String, List<String>> SELECT_ID_URI_LIST_MAP = new ConcurrentHashMap<>();

    private static final Map<String, GenericApiConfig> SELECT_API_CONFIG_MAP = new ConcurrentHashMap<>();

    private final Class<C> clazz;

    protected AbstractPluginDataHandler(final Class<C> clazz) {
        this.clazz = clazz;
    }

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

    @Override
    public void handlerSelector(final SelectorData selectorData) {
        LOG.info("handler {} selector data:{}", pluginNamed(), GsonUtils.getGson().toJson(selectorData));
        String handleJson = selectorData.getHandle();
        if (StringUtils.isEmpty(handleJson) || EMPTY_JSON.equals(handleJson.trim())) {
            return;
        }
        if (selectorData.getType() != SelectorTypeEnum.CUSTOM_FLOW.getCode()
            || CollectionUtils.isEmpty(selectorData.getConditionList())) {
            return;
        }
        GenericApiConfig logApiConfig = GsonUtils.getInstance().fromJson(handleJson, clazz);
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
}
