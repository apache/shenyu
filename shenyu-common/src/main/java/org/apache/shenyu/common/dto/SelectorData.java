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

package org.apache.shenyu.common.dto;

import java.util.List;
import java.util.Objects;

/**
 * SelectorData.
 *
 * @since 2.0.0
 */
public class SelectorData extends BaseData {

    private String pluginId;

    /**
     * plugin name.
     */
    private String pluginName;

    /**
     * matchMode（0 and  1 or).
     */
    private Integer matchMode;

    /**
     * type（false full，true custom).
     */
    private Integer type;

    private Boolean logged;

    private Boolean continued = Boolean.TRUE;

    private String handle;

    private List<ConditionData> conditionList;

    private List<ConditionData> beforeConditionList;

    /**
     * match restful api.
     */
    private Boolean matchRestful;

    /**
     * no args constructor.
     */
    public SelectorData() {
    }

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private SelectorData(final Builder builder) {
        this.setId(builder.id);
        this.pluginId = builder.pluginId;
        this.pluginName = builder.pluginName;
        this.setName(builder.name);
        this.matchMode = builder.matchMode;
        this.type = builder.type;
        this.setSort(builder.sort);
        this.setEnabled(builder.enabled);
        this.logged = builder.logged;
        this.continued = builder.continued;
        this.handle = builder.handle;
        this.conditionList = builder.conditionList;
        this.matchRestful = builder.matchRestful;
        this.beforeConditionList = builder.beforeConditionList;
        this.setNamespaceId(builder.namespaceId);
    }

    /**
     * class builder.
     *
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
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
        if (Objects.isNull(continued)) {
            return true;
        }
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

    /**
     * get match restful.
     *
     * @return match restful
     */
    public Boolean getMatchRestful() {
        return matchRestful;
    }

    /**
     * set match restful.
     *
     * @param matchRestful matchRestful
     */
    public void setMatchRestful(final Boolean matchRestful) {
        this.matchRestful = matchRestful;
    }

    /**
     * get before condition list.
     *
     * @return before condition list
     */
    public List<ConditionData> getBeforeConditionList() {
        return beforeConditionList;
    }

    /**
     * set before condition list.
     *
     * @param beforeConditionList before condition list
     */
    public void setBeforeConditionList(final List<ConditionData> beforeConditionList) {
        this.beforeConditionList = beforeConditionList;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        SelectorData that = (SelectorData) o;
        return Objects.equals(getId(), that.getId()) && Objects.equals(pluginId, that.pluginId) && Objects.equals(pluginName, that.pluginName)
                && Objects.equals(getName(), that.getName()) && Objects.equals(matchMode, that.matchMode) && Objects.equals(type, that.type)
                && Objects.equals(getSort(), that.getSort()) && Objects.equals(getEnabled(), that.getEnabled()) && Objects.equals(logged, that.logged)
                && Objects.equals(continued, that.continued) && Objects.equals(handle, that.handle) && Objects.equals(conditionList, that.conditionList)
                && Objects.equals(beforeConditionList, that.beforeConditionList) && Objects.equals(matchRestful, that.matchRestful)
                && Objects.equals(getNamespaceId(), that.getNamespaceId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getId(), pluginId, pluginName, getName(), matchMode, type, getSort(), getEnabled(), logged, continued, handle,
                conditionList, beforeConditionList, matchRestful, getNamespaceId());
    }

    @Override
    public String toString() {
        return "SelectorData{"
                + "id='"
                + getId()
                + '\''
                + ", pluginId='"
                + pluginId
                + '\''
                + ", pluginName='"
                + pluginName
                + '\''
                + ", name='"
                + getName()
                + '\''
                + ", matchMode="
                + matchMode
                + ", type="
                + type
                + ", sort="
                + getSort()
                + ", enabled="
                + getEnabled()
                + ", logged="
                + logged
                + ", continued="
                + continued
                + ", handle='"
                + handle
                + '\''
                + ", conditionList="
                + conditionList
                + ", matchRestful="
                + matchRestful
                + ", namespaceId="
                + getNamespaceId()
                + '}';
    }

    /**
     * class builder.
     */
    public static final class Builder {

        private String id;

        private String name;

        private String pluginId;

        private String pluginName;

        private Integer matchMode;

        private Integer type;

        private Integer sort;

        private Boolean enabled;

        private Boolean logged;

        private Boolean continued;

        private String handle;

        private List<ConditionData> conditionList;

        private Boolean matchRestful;

        private List<ConditionData> beforeConditionList;

        private String namespaceId;

        /**
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return SelectorData
         */
        public SelectorData build() {
            return new SelectorData(this);
        }

        /**
         * build id.
         *
         * @param id id
         * @return this
         */
        public Builder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * build pluginId.
         *
         * @param pluginId pluginId
         * @return this
         */
        public Builder pluginId(final String pluginId) {
            this.pluginId = pluginId;
            return this;
        }

        /**
         * build pluginName.
         *
         * @param pluginName pluginName
         * @return this
         */
        public Builder pluginName(final String pluginName) {
            this.pluginName = pluginName;
            return this;
        }

        /**
         * build name.
         *
         * @param name name
         * @return this
         */
        public Builder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * build matchMode.
         *
         * @param matchMode matchMode
         * @return this
         */
        public Builder matchMode(final Integer matchMode) {
            this.matchMode = matchMode;
            return this;
        }

        /**
         * build type.
         *
         * @param type type
         * @return this
         */
        public Builder type(final Integer type) {
            this.type = type;
            return this;
        }

        /**
         * build sort.
         *
         * @param sort sort
         * @return this
         */
        public Builder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }

        /**
         * build enabled.
         *
         * @param enabled enabled
         * @return this
         */
        public Builder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * build logged.
         *
         * @param logged logged
         * @return this
         */
        public Builder logged(final Boolean logged) {
            this.logged = logged;
            return this;
        }

        /**
         * build continued.
         *
         * @param continued continued
         * @return this
         */
        public Builder continued(final Boolean continued) {
            this.continued = continued;
            return this;
        }

        /**
         * build handle.
         *
         * @param handle handle
         * @return this
         */
        public Builder handle(final String handle) {
            this.handle = handle;
            return this;
        }

        /**
         * build conditionList.
         *
         * @param conditionList conditionList
         * @return this
         */
        public Builder conditionList(final List<ConditionData> conditionList) {
            this.conditionList = conditionList;
            return this;
        }

        /**
         * build match restful.
         *
         * @param matchRestful matchRestful
         * @return this
         */
        public Builder matchRestful(final Boolean matchRestful) {
            this.matchRestful = matchRestful;
            return this;
        }

        /**
         * build beforeConditionList.
         *
         * @param beforeConditionList beforeConditionList
         * @return this
         */
        public Builder beforeConditionList(final List<ConditionData> beforeConditionList) {
            this.beforeConditionList = beforeConditionList;
            return this;
        }

        /**
         * build namespaceId.
         *
         * @param namespaceId namespaceId
         * @return this
         */
        public Builder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }
    }
}
