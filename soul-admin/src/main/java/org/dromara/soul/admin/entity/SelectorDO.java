/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.admin.entity;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.SelectorDTO;
import org.dromara.soul.common.dto.ConditionData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.List;

/**
 * SelectorDO.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Data
public class SelectorDO extends BaseDO {

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
        if (selectorDTO != null) {
            SelectorDO selectorDO = new SelectorDO();
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            if (StringUtils.isEmpty(selectorDTO.getId())) {
                selectorDO.setId(UUIDUtils.getInstance().generateShortUuid());
                selectorDO.setDateCreated(currentTime);
            } else {
                selectorDO.setId(selectorDTO.getId());
            }

            selectorDO.setPluginId(selectorDTO.getPluginId());
            selectorDO.setName(selectorDTO.getName());
            selectorDO.setMatchMode(selectorDTO.getMatchMode());
            selectorDO.setType(selectorDTO.getType());
            selectorDO.setSort(selectorDTO.getSort());
            selectorDO.setEnabled(selectorDTO.getEnabled());
            selectorDO.setLoged(selectorDTO.getLoged());
            selectorDO.setContinued(selectorDTO.getContinued());
            selectorDO.setDateUpdated(currentTime);
            selectorDO.setHandle(selectorDTO.getHandle());
            return selectorDO;
        }
        return null;
    }

    public static SelectorData transFrom(final SelectorDO selectorDO, final String pluginName, final List<ConditionData> conditionDataList) {
        return new SelectorData(selectorDO.getId(),
                selectorDO.getPluginId(),
                pluginName,
                selectorDO.getName(),
                selectorDO.getMatchMode(),
                selectorDO.getType(),
                selectorDO.getSort(),
                selectorDO.getEnabled(),
                selectorDO.getLoged(),
                selectorDO.getContinued(),
                selectorDO.getHandle(),
                conditionDataList);
    }
}
