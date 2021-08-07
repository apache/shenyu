/*
 *
 *  * Licensed to the Apache Software Foundation (ASF) under one or more
 *  * contributor license agreements.  See the NOTICE file distributed with
 *  * this work for additional information regarding copyright ownership.
 *  * The ASF licenses this file to You under the Apache License, Version 2.0
 *  * (the "License"); you may not use this file except in compliance with
 *  * the License.  You may obtain a copy of the License at
 *  *
 *  *     http://www.apache.org/licenses/LICENSE-2.0
 *  *
 *  * Unless required by applicable law or agreed to in writing, software
 *  * distributed under the License is distributed on an "AS IS" BASIS,
 *  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  * See the License for the specific language governing permissions and
 *  * limitations under the License.
 *
 */

package org.apache.shenyu.common.dto;

import java.util.List;
import java.util.Objects;

/**
 * RuleDTO.
 *
 * @since 2.0.0
 */

public class RuleData {

    private String id;

    private String name;

    private String pluginName;

    private String selectorId;

    /**
     * match way（0 and  1 or).
     */
    private Integer matchMode;

    private Integer sort;

    private Boolean enabled;

    private Boolean loged;

    /**
     * handle message（different plugin have different handle to mark ,json style）.
     */
    private String handle;

    private List<ConditionData> conditionDataList;

    /**
     * no args constructor.
     */
    public RuleData() {
    }

    /**
     * all args constructor.
     *
     * @param id                id
     * @param name              name
     * @param pluginName        pluginName
     * @param selectorId        selectorId
     * @param matchMode         matchMode
     * @param sort              sort
     * @param enabled           enabled
     * @param loged             loged
     * @param handle            handle
     * @param conditionDataList conditionDataList
     */
    public RuleData(final String id, final String name, final String pluginName, final String selectorId, final Integer matchMode,
                    final Integer sort, final Boolean enabled, final Boolean loged, final String handle, final List<ConditionData> conditionDataList) {
        this.id = id;
        this.name = name;
        this.pluginName = pluginName;
        this.selectorId = selectorId;
        this.matchMode = matchMode;
        this.sort = sort;
        this.enabled = enabled;
        this.loged = loged;
        this.handle = handle;
        this.conditionDataList = conditionDataList;
    }

    /**
     * get id.
     *
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * set id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * get name.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * set name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get pluginName.
     *
     * @return pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * set pluginName.
     *
     * @param pluginName pluginName
     */
    public void setPluginName(final String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * get selectorId.
     *
     * @return selectorId
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * set selectorId.
     *
     * @param selectorId selectorId
     */
    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
    }

    /**
     * get matchMode.
     *
     * @return matchMode
     */
    public Integer getMatchMode() {
        return matchMode;
    }

    /**
     * set matchMode.
     *
     * @param matchMode matchMode
     */
    public void setMatchMode(final Integer matchMode) {
        this.matchMode = matchMode;
    }

    /**
     * get sort.
     *
     * @return sort
     */
    public Integer getSort() {
        return sort;
    }

    /**
     * set sort.
     *
     * @param sort sort
     */
    public void setSort(final Integer sort) {
        this.sort = sort;
    }

    /**
     * get enabled.
     *
     * @return enabled
     */
    public Boolean getEnabled() {
        return enabled;
    }

    /**
     * set enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * get loged.
     *
     * @return loged
     */
    public Boolean getLoged() {
        return loged;
    }

    /**
     * set loged.
     *
     * @param loged loged
     */
    public void setLoged(final Boolean loged) {
        this.loged = loged;
    }

    /**
     * get handle.
     *
     * @return handle
     */
    public String getHandle() {
        return handle;
    }

    /**
     * set handle.
     *
     * @param handle handle
     */
    public void setHandle(final String handle) {
        this.handle = handle;
    }

    /**
     * get conditionDataList.
     *
     * @return conditionDataList
     */
    public List<ConditionData> getConditionDataList() {
        return conditionDataList;
    }

    /**
     * set conditionDataList.
     *
     * @param conditionDataList conditionDataList
     */
    public void setConditionDataList(final List<ConditionData> conditionDataList) {
        this.conditionDataList = conditionDataList;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        RuleData ruleData = (RuleData) o;
        return Objects.equals(id, ruleData.id)
                && Objects.equals(name, ruleData.name)
                && Objects.equals(pluginName, ruleData.pluginName)
                && Objects.equals(selectorId, ruleData.selectorId)
                && Objects.equals(matchMode, ruleData.matchMode)
                && Objects.equals(sort, ruleData.sort)
                && Objects.equals(enabled, ruleData.enabled)
                && Objects.equals(loged, ruleData.loged)
                && Objects.equals(handle, ruleData.handle)
                && Objects.equals(conditionDataList, ruleData.conditionDataList);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, name, pluginName, selectorId, matchMode, sort, enabled, loged, handle, conditionDataList);
    }

    @Override
    public String toString() {
        return "RuleData{"
                + "id='"
                + id
                + '\''
                + ", name='"
                + name
                + '\''
                + ", pluginName='"
                + pluginName
                + '\''
                + ", selectorId='"
                + selectorId
                + '\''
                + ", matchMode="
                + matchMode
                + ", sort="
                + sort
                + ", enabled="
                + enabled
                + ", loged="
                + loged
                + ", handle='"
                + handle
                + '\''
                + ", conditionDataList="
                + conditionDataList
                + '}';
    }
}
