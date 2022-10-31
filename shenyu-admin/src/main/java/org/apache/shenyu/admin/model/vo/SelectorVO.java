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

import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.List;

/**
 * this is selector view to web front.
 */
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

    public SelectorVO() {
    }

    public SelectorVO(final String id,
                      final String pluginId,
                      final String name,
                      final Integer matchMode,
                      final String matchModeName,
                      final Integer type,
                      final String typeName,
                      final Integer sort,
                      final Boolean enabled,
                      final Boolean loged,
                      final Boolean continued,
                      final String handle,
                      final List<SelectorConditionVO> selectorConditions,
                      final String dateCreated,
                      final String dateUpdated) {
        this.id = id;
        this.pluginId = pluginId;
        this.name = name;
        this.matchMode = matchMode;
        this.matchModeName = matchModeName;
        this.type = type;
        this.typeName = typeName;
        this.sort = sort;
        this.enabled = enabled;
        this.loged = loged;
        this.continued = continued;
        this.handle = handle;
        this.selectorConditions = selectorConditions;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
    }

    /**
     * Gets the value of id.
     *
     * @return the value of id
     */
    public String getId() {
        return id;
    }

    /**
     * Sets the id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * Gets the value of pluginId.
     *
     * @return the value of pluginId
     */
    public String getPluginId() {
        return pluginId;
    }

    /**
     * Sets the pluginId.
     *
     * @param pluginId pluginId
     */
    public void setPluginId(final String pluginId) {
        this.pluginId = pluginId;
    }

    /**
     * Gets the value of name.
     *
     * @return the value of name
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * Gets the value of matchMode.
     *
     * @return the value of matchMode
     */
    public Integer getMatchMode() {
        return matchMode;
    }

    /**
     * Sets the matchMode.
     *
     * @param matchMode matchMode
     */
    public void setMatchMode(final Integer matchMode) {
        this.matchMode = matchMode;
    }

    /**
     * Gets the value of matchModeName.
     *
     * @return the value of matchModeName
     */
    public String getMatchModeName() {
        return matchModeName;
    }

    /**
     * Sets the matchModeName.
     *
     * @param matchModeName matchModeName
     */
    public void setMatchModeName(final String matchModeName) {
        this.matchModeName = matchModeName;
    }

    /**
     * Gets the value of type.
     *
     * @return the value of type
     */
    public Integer getType() {
        return type;
    }

    /**
     * Sets the type.
     *
     * @param type type
     */
    public void setType(final Integer type) {
        this.type = type;
    }

    /**
     * Gets the value of typeName.
     *
     * @return the value of typeName
     */
    public String getTypeName() {
        return typeName;
    }

    /**
     * Sets the typeName.
     *
     * @param typeName typeName
     */
    public void setTypeName(final String typeName) {
        this.typeName = typeName;
    }

    /**
     * Gets the value of sort.
     *
     * @return the value of sort
     */
    public Integer getSort() {
        return sort;
    }

    /**
     * Sets the sort.
     *
     * @param sort sort
     */
    public void setSort(final Integer sort) {
        this.sort = sort;
    }

    /**
     * Gets the value of enabled.
     *
     * @return the value of enabled
     */
    public Boolean getEnabled() {
        return enabled;
    }

    /**
     * Sets the enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Gets the value of loged.
     *
     * @return the value of loged
     */
    public Boolean getLoged() {
        return loged;
    }

    /**
     * Sets the loged.
     *
     * @param loged loged
     */
    public void setLoged(final Boolean loged) {
        this.loged = loged;
    }

    /**
     * Gets the value of continued.
     *
     * @return the value of continued
     */
    public Boolean getContinued() {
        return continued;
    }

    /**
     * Sets the continued.
     *
     * @param continued continued
     */
    public void setContinued(final Boolean continued) {
        this.continued = continued;
    }

    /**
     * Gets the value of handle.
     *
     * @return the value of handle
     */
    public String getHandle() {
        return handle;
    }

    /**
     * Sets the handle.
     *
     * @param handle handle
     */
    public void setHandle(final String handle) {
        this.handle = handle;
    }

    /**
     * Gets the value of selectorConditions.
     *
     * @return the value of selectorConditions
     */
    public List<SelectorConditionVO> getSelectorConditions() {
        return selectorConditions;
    }

    /**
     * Sets the selectorConditions.
     *
     * @param selectorConditions selectorConditions
     */
    public void setSelectorConditions(final List<SelectorConditionVO> selectorConditions) {
        this.selectorConditions = selectorConditions;
    }

    /**
     * Gets the value of dateCreated.
     *
     * @return the value of dateCreated
     */
    public String getDateCreated() {
        return dateCreated;
    }

    /**
     * Sets the dateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final String dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * Gets the value of dateUpdated.
     *
     * @return the value of dateUpdated
     */
    public String getDateUpdated() {
        return dateUpdated;
    }

    /**
     * Sets the dateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final String dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

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
