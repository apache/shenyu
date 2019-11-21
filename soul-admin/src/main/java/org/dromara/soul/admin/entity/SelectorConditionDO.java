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
import org.dromara.soul.admin.dto.SelectorConditionDTO;
import org.dromara.soul.common.utils.UUIDUtils;

import java.sql.Timestamp;

/**
 * SelectorConditionDO.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Data
public class SelectorConditionDO extends BaseDO {

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * parameter type.
     */
    private String paramType;

    /**
     * match operator.
     */
    private String operator;

    /**
     * parameter name.
     */
    private String paramName;

    /**
     * parameter value.
     */
    private String paramValue;

    /**
     * build selectorConditionDO.
     *
     * @param selectorConditionDTO {@linkplain SelectorConditionDTO}
     * @return {@linkplain SelectorConditionDO}
     */
    public static SelectorConditionDO buildSelectorConditionDO(final SelectorConditionDTO selectorConditionDTO) {
        if (selectorConditionDTO != null) {
            SelectorConditionDO selectorConditionDO = new SelectorConditionDO();
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            if (StringUtils.isEmpty(selectorConditionDTO.getId())) {
                selectorConditionDO.setId(UUIDUtils.getInstance().generateShortUuid());
                selectorConditionDO.setDateCreated(currentTime);
            } else {
                selectorConditionDO.setId(selectorConditionDTO.getId());
            }

            selectorConditionDO.setParamType(selectorConditionDTO.getParamType());
            selectorConditionDO.setSelectorId(selectorConditionDTO.getSelectorId());
            selectorConditionDO.setOperator(selectorConditionDTO.getOperator());
            selectorConditionDO.setParamName(selectorConditionDTO.getParamName());
            selectorConditionDO.setParamValue(selectorConditionDTO.getParamValue());
            selectorConditionDO.setDateUpdated(currentTime);
            return selectorConditionDO;
        }
        return null;
    }
}
