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

package org.apache.shenyu.admin.model.entity;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;

/**
 * SelectorDO.
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class SelectorDO extends BaseDO {

    private static final long serialVersionUID = -1627940797162331235L;

    /**
     * plugin id.
     */
    private String pluginId;

    /**
     * selector name.
     */
    private String name;

    /**
     * match mode.
     */
    private Integer matchMode;

    /**
     * selector type.
     */
    private Integer type;

    /**
     * sort type.
     */
    private Integer sort;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * whether loged.
     */
    private Boolean loged;

    /**
     * whether continued.
     */
    private Boolean continued;

    private String handle;

    /**
     * build selectorDO.
     *
     * @param selectorDTO {@linkplain SelectorDTO}
     * @return {@linkplain SelectorDO}
     */
    public static SelectorDO buildSelectorDO(final SelectorDTO selectorDTO) {
        return Optional.ofNullable(selectorDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            SelectorDO selectorDO = SelectorDO.builder()
                    .type(item.getType())
                    .sort(item.getSort())
                    .enabled(item.getEnabled())
                    .loged(item.getLoged())
                    .continued(item.getContinued())
                    .dateUpdated(currentTime)
                    .handle(item.getHandle())
                    .pluginId(item.getPluginId())
                    .name(item.getName())
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                selectorDO.setId(UUIDUtils.getInstance().generateShortUuid());
                selectorDO.setDateCreated(currentTime);
            } else {
                selectorDO.setId(item.getId());
            }
            if (SelectorTypeEnum.FULL_FLOW.getCode() == item.getType()) {
                selectorDO.setMatchMode(MatchModeEnum.AND.getCode());
            } else {
                selectorDO.setMatchMode(item.getMatchMode());
            }
            return selectorDO;
        }).orElse(null);
    }

    /**
     * Trans from selector data.
     *
     * @param selectorDO        the selector do
     * @param pluginName        the plugin name
     * @param conditionDataList the condition data list
     * @return the selector data
     */
    public static SelectorData transFrom(final SelectorDO selectorDO, final String pluginName, final List<ConditionData> conditionDataList) {
        return SelectorData.builder()
                .id(selectorDO.getId())
                .pluginId(selectorDO.getPluginId())
                .pluginName(pluginName)
                .name(selectorDO.getName())
                .matchMode(selectorDO.getMatchMode())
                .type(selectorDO.getType())
                .sort(selectorDO.getSort())
                .enabled(selectorDO.getEnabled())
                .logged(selectorDO.getLoged())
                .continued(selectorDO.getContinued())
                .handle(selectorDO.getHandle())
                .conditionList(conditionDataList)
                .build();
    }
}
