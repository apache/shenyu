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

package org.apache.shenyu.admin.model.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.List;

/**
 * this is selector view to web front.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class SelectorVO implements Serializable {

    private static final long serialVersionUID = -8025780005899060366L;

    /**
     * primary key.
     */
    private String id;

    /**
     * plugin id.
     */
    private String pluginId;

    /**
     * selector name.
     */
    private String name;

    /**
     * match mode code.
     */
    private Integer matchMode;

    /**
     * match mode name.
     */
    private String matchModeName;

    /**
     * selector type code.
     */
    private Integer type;

    /**
     * selector type name.
     */
    private String typeName;

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
     * selector conditions.
     */
    private List<SelectorConditionVO> selectorConditions;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    /**
     * build selectorVO.
     *
     * @param selectorDO {@linkplain SelectorDO}
     * @return {@linkplain SelectorVO}
     */
    public static SelectorVO buildSelectorVO(final SelectorDO selectorDO) {
        return buildSelectorVO(selectorDO, null);
    }

    /**
     * build selectorVO.
     *
     * @param selectorDO         {@linkplain SelectorDO}
     * @param selectorConditions {@linkplain List}
     * @return {@linkplain SelectorVO}
     */
    public static SelectorVO buildSelectorVO(final SelectorDO selectorDO, final List<SelectorConditionVO> selectorConditions) {
        return new SelectorVO(selectorDO.getId(), selectorDO.getPluginId(), selectorDO.getName(), selectorDO.getMatchMode(), MatchModeEnum.getMatchModeByCode(selectorDO.getMatchMode()),
                selectorDO.getType(), SelectorTypeEnum.getSelectorTypeByCode(selectorDO.getType()), selectorDO.getSort(),
                selectorDO.getEnabled(), selectorDO.getLoged(), selectorDO.getContinued(), selectorDO.getHandle(), selectorConditions,
                DateUtils.localDateTimeToString(selectorDO.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(selectorDO.getDateUpdated().toLocalDateTime()));
    }
}
