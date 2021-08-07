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
 * SelectorDTO.
 *
 * @since 2.0.0
 */
public class SelectorData {

    private String id;

    private String pluginId;

    /**
     * plugin name.
     */
    private String pluginName;

    private String name;

    /**
     * matchMode（0 and  1 or).
     */
    private Integer matchMode;

    /**
     * type（false full，true custom).
     */
    private Integer type;

    private Integer sort;

    private Boolean enabled;

    private Boolean logged;

    private Boolean continued;

    private String handle;

    private List<ConditionData> conditionList;

    /**
     * no args constructor.
     */
    public SelectorData() {
    }

    /**
     * all args constructor.
     *
     * @param id            id
     * @param pluginId      pluginId
     * @param pluginName    pluginName
     * @param name          name
     * @param matchMode     matchMode
     * @param type          type
     * @param sort          sort
     * @param enabled       enabled
     * @param logged        logged
     * @param continued     continued
     * @param handle        handle
     * @param conditionList conditionList
     */
    public SelectorData(final String id, final String pluginId, final String pluginName, final String name,
                        final Integer matchMode, final Integer type, final Integer sort, final Boolean enabled,
                        final Boolean logged, final Boolean continued, final String handle, final List<ConditionData> conditionList) {
        this.id = id;
        this.pluginId = pluginId;
        this.pluginName = pluginName;
        this.name = name;
        this.matchMode = matchMode;
        this.type = type;
        this.sort = sort;
        this.enabled = enabled;
        this.logged = logged;
        this.continued = continued;
        this.handle = handle;
        this.conditionList = conditionList;
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
     * get pluginId.
     *
     * @return pluginId
     */
    public String getPluginId() {
        return pluginId;
    }

    /**
     * set pluginId.
     *
     * @param pluginId pluginId
     */
    public void setPluginId(final String pluginId) {
        this.pluginId = pluginId;
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
     * get type.
     *
     * @return type
     */
    public Integer getType() {
        return type;
    }

    /**
     * set type.
     *
     * @param type type
     */
    public void setType(final Integer type) {
        this.type = type;
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
     * get logged.
     *
     * @return logged
     */
    public Boolean getLogged() {
        return logged;
    }

    /**
     * set logged.
     *
     * @param logged logged
     */
    public void setLogged(final Boolean logged) {
        this.logged = logged;
    }

    /**
     * get continued.
     *
     * @return continued
     */
    public Boolean getContinued() {
        return continued;
    }

    /**
     * set continued.
     *
     * @param continued continued
     */
    public void setContinued(final Boolean continued) {
        this.continued = continued;
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
     * get conditionList.
     *
     * @return conditionList
     */
    public List<ConditionData> getConditionList() {
        return conditionList;
    }

    /**
     * set conditionList.
     *
     * @param conditionList conditionList
     */
    public void setConditionList(final List<ConditionData> conditionList) {
        this.conditionList = conditionList;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        SelectorData that = (SelectorData) o;
        return Objects.equals(id, that.id)
                && Objects.equals(pluginId, that.pluginId)
                && Objects.equals(pluginName, that.pluginName)
                && Objects.equals(name, that.name)
                && Objects.equals(matchMode, that.matchMode)
                && Objects.equals(type, that.type)
                && Objects.equals(sort, that.sort)
                && Objects.equals(enabled, that.enabled)
                && Objects.equals(logged, that.logged)
                && Objects.equals(continued, that.continued)
                && Objects.equals(handle, that.handle)
                && Objects.equals(conditionList, that.conditionList);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, pluginId, pluginName, name, matchMode, type, sort, enabled, logged, continued, handle, conditionList);
    }

    @Override
    public String toString() {
        return "SelectorData{"
                + "id='"
                + id
                + '\''
                + ", pluginId='"
                + pluginId
                + '\''
                + ", pluginName='"
                + pluginName
                + '\''
                + ", name='"
                + name
                + '\''
                + ", matchMode="
                + matchMode
                + ", type="
                + type
                + ", sort="
                + sort
                + ", enabled="
                + enabled
                + ", logged="
                + logged
                + ", continued="
                + continued
                + ", handle='"
                + handle
                + '\''
                + ", conditionList="
                + conditionList
                + '}';
    }
}
