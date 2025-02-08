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

package org.apache.shenyu.e2e.model.data;

import java.util.List;
import java.util.Objects;

/**
 * RuleCacheData.
 *
 * @since 2.0.0
 */
public class RuleCacheData {
    
    /**
     * id.
     */
    private String id;
    
    /**
     * name.
     */
    private String name;
    
    /**
     * plugin name.
     */
    private String pluginName;
    
    /**
     * selector id.
     */
    private String selectorId;

    /**
     * match way（0 or 1).
     */
    private Integer matchMode;
    
    /**
     * sort.
     */
    private Integer sort;
    
    /**
     * enabled or not.
     */
    private Boolean enabled;
    
    /**
     * logged.
     */
    private Boolean loged;

    /**
     * handle message（different plugin have different handle to mark ,json style）.
     */
    private String handle;
    
    /**
     * condition data list.
     */
    private List<ConditionData> conditionDataList;
    
    /**
     * before condition list.
     */
    private List<ConditionData> beforeConditionDataList;

    /**
     * match restful.
     */
    private Boolean matchRestful;
    
    /**
     * namespaceId.
     */
    private String namespaceId;

    /**
     * no args constructor.
     */
    public RuleCacheData() {
    }

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private RuleCacheData(final Builder builder) {
        this.id = builder.id;
        this.name = builder.name;
        this.pluginName = builder.pluginName;
        this.selectorId = builder.selectorId;
        this.matchMode = builder.matchMode;
        this.sort = builder.sort;
        this.enabled = builder.enabled;
        this.loged = builder.loged;
        this.handle = builder.handle;
        this.conditionDataList = builder.conditionDataList;
        this.beforeConditionDataList = builder.beforeConditionDataList;
        this.matchRestful = builder.matchRestful;
        this.namespaceId = builder.namespaceId;
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
     * @return this
     */
    public RuleCacheData setId(final String id) {
        this.id = id;
        return this;
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
     * @return this
     */
    public RuleCacheData setName(final String name) {
        this.name = name;
        return this;
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
     * @return this
     */
    public RuleCacheData setPluginName(final String pluginName) {
        this.pluginName = pluginName;
        return this;
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
     * @return this
     */
    public RuleCacheData setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
        return this;
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
     * @return this
     */
    public RuleCacheData setMatchMode(final Integer matchMode) {
        this.matchMode = matchMode;
        return this;
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
     * @return this
     */
    public RuleCacheData setSort(final Integer sort) {
        this.sort = sort;
        return this;
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
     * @return this
     */
    public RuleCacheData setEnabled(final Boolean enabled) {
        this.enabled = enabled;
        return this;
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
     * @return this
     */
    public RuleCacheData setLogged(final Boolean loged) {
        this.loged = loged;
        return this;
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
     * @return this
     */
    public RuleCacheData setHandle(final String handle) {
        this.handle = handle;
        return this;
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
     * @return this
     */
    public RuleCacheData setConditionDataList(final List<ConditionData> conditionDataList) {
        this.conditionDataList = conditionDataList;
        return this;
    }

    /**
     * get beforeConditionDataList.
     *
     * @return beforeConditionDataList
     */
    public List<ConditionData> getBeforeConditionDataList() {
        return beforeConditionDataList;
    }

    /**
     * set beforeConditionDataList.
     *
     * @param beforeConditionDataList beforeConditionDataList
     */
    public void setBeforeConditionDataList(final List<ConditionData> beforeConditionDataList) {
        this.beforeConditionDataList = beforeConditionDataList;
    }
    
    /**
     * get match restful.
     *
     * @return matchRestful
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
     * get namespaceId.
     *
     * @return namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }
    
    /**
     * set namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        final RuleCacheData ruleCacheData = (RuleCacheData) o;
        return Objects.equals(id, ruleCacheData.id)
                && Objects.equals(name, ruleCacheData.name)
                && Objects.equals(pluginName, ruleCacheData.pluginName)
                && Objects.equals(selectorId, ruleCacheData.selectorId)
                && Objects.equals(matchMode, ruleCacheData.matchMode)
                && Objects.equals(sort, ruleCacheData.sort)
                && Objects.equals(enabled, ruleCacheData.enabled)
                && Objects.equals(loged, ruleCacheData.loged)
                && Objects.equals(handle, ruleCacheData.handle)
                && Objects.equals(conditionDataList, ruleCacheData.conditionDataList)
                && Objects.equals(beforeConditionDataList, ruleCacheData.beforeConditionDataList)
                && Objects.equals(matchRestful, ruleCacheData.matchRestful)
                && Objects.equals(namespaceId, ruleCacheData.namespaceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, name, pluginName, selectorId, matchMode, sort, enabled, loged, handle, conditionDataList,
                beforeConditionDataList, matchRestful, namespaceId);
    }

    @Override
    public String toString() {
        return "RuleCacheData{"
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
                + ", matchRestful="
                + matchRestful
                + ", namespaceId="
                + namespaceId
                + '}';
    }

    /**
     * class builder.
     */
    public static final class Builder {

        private String id;

        private String name;

        private String pluginName;

        private String selectorId;

        private Integer matchMode;

        private Integer sort;

        private Boolean enabled;

        private Boolean loged;

        private String handle;

        private List<ConditionData> conditionDataList;

        private List<ConditionData> beforeConditionDataList;
        
        private Boolean matchRestful;
        
        private String namespaceId;

        /**
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return RuleCacheData
         */
        public RuleCacheData build() {
            return new RuleCacheData(this);
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
         * build selectorId.
         *
         * @param selectorId selectorId
         * @return this
         */
        public Builder selectorId(final String selectorId) {
            this.selectorId = selectorId;
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
         * build loged.
         *
         * @param loged loged
         * @return this
         */
        public Builder loged(final Boolean loged) {
            this.loged = loged;
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
         * build conditionDataList.
         *
         * @param conditionDataList conditionDataList
         * @return this
         */
        public Builder conditionDataList(final List<ConditionData> conditionDataList) {
            this.conditionDataList = conditionDataList;
            return this;
        }

        /**
         * build conditionDataList.
         *
         * @param beforeConditionDataList beforeConditionDataList
         * @return this
         */
        public Builder beforeConditionDataList(final List<ConditionData> beforeConditionDataList) {
            this.beforeConditionDataList = beforeConditionDataList;
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
