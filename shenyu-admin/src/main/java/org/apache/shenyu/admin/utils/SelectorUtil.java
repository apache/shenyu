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

package org.apache.shenyu.admin.utils;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * ResourceUtil.
 */
public final class SelectorUtil {
    
    private SelectorUtil() {
    }
    
    
    /**
     * build divide upstream<br>.
     * if plugin is spring-cloud used {@link SpringCloudSelectorHandle}.<br>
     * if plugin is divide used {@link DivideUpstream}.<br>
     *
     * @param selectorDO selector
     * @param pluginName plugin name
     * @return default is empty list.
     */
    public static List<DivideUpstream> buildDivideUpstream(final SelectorDO selectorDO, final String pluginName) {
        if (PluginEnum.SPRING_CLOUD.getName().equals(pluginName) && Objects.nonNull(selectorDO.getHandle())) {
            return GsonUtils.getInstance()
                    .fromJson(selectorDO.getHandle(), SpringCloudSelectorHandle.class)
                    .getDivideUpstreams();
        }
        if (PluginEnum.DIVIDE.getName().equals(pluginName) && StringUtils.isNotBlank(selectorDO.getHandle())) {
            return GsonUtils.getInstance()
                    .fromList(selectorDO.getHandle(), DivideUpstream.class);
        }
        return Collections.emptyList();
    }
    
    /**
     * build selector.
     *
     * @param contextPath context path
     * @param pluginId    plugin id
     * @return selector
     */
    public static SelectorDTO buildSelectorDTO(final String contextPath, final String pluginId) {
        SelectorDTO selectorDTO = buildDefaultSelectorDTO(contextPath);
        selectorDTO.setPluginId(pluginId);
        selectorDTO.setSelectorConditions(buildDefaultSelectorConditionDTO(contextPath));
        return selectorDTO;
    }
    
    /**
     * build default selector.
     *
     * @param name selector name
     * @return selector
     */
    public static SelectorDTO buildDefaultSelectorDTO(final String name) {
        return SelectorDTO.builder()
                .name(name)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .matchMode(MatchModeEnum.AND.getCode())
                .enabled(Boolean.TRUE)
                .loged(Boolean.TRUE)
                .continued(Boolean.TRUE)
                .matchRestful(Boolean.FALSE)
                .sort(1)
                .build();
    }
    
    /**
     * build default selector condition list.
     *
     * @param contextPath context path
     * @return list
     */
    public static List<SelectorConditionDTO> buildDefaultSelectorConditionDTO(final String contextPath) {
        SelectorConditionDTO selectorConditionDTO = new SelectorConditionDTO();
        selectorConditionDTO.setParamType(ParamTypeEnum.URI.getName());
        selectorConditionDTO.setParamName("/");
        selectorConditionDTO.setOperator(OperatorEnum.STARTS_WITH.getAlias());
        selectorConditionDTO.setParamValue(contextPath + Constants.PATH_SEPARATOR);
        return Collections.singletonList(selectorConditionDTO);
    }
}
