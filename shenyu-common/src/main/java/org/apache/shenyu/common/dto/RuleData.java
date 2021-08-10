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
     * builder constructor.
     *
     * @param builder builder
     */
    private RuleData(final Builder builder) {
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
    public RuleData setId(final String id) {
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
    public RuleData setName(final String name) {
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
    public RuleData setPluginName(final String pluginName) {
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
    public RuleData setSelectorId(final String selectorId) {
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
    public RuleData setMatchMode(final Integer matchMode) {
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
    public RuleData setSort(final Integer sort) {
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
    public RuleData setEnabled(final Boolean enabled) {
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
    public RuleData setLoged(final Boolean loged) {
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
    public RuleData setHandle(final String handle) {
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
    public RuleData setConditionDataList(final List<ConditionData> conditionDataList) {
        this.conditionDataList = conditionDataList;
        return this;
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
        return Objects.equals(id, ruleData.id) && Objects.equals(name, ruleData.name) && Objects.equals(pluginName, ruleData.pluginName)
                && Objects.equals(selectorId, ruleData.selectorId) && Objects.equals(matchMode, ruleData.matchMode)
                && Objects.equals(sort, ruleData.sort) && Objects.equals(enabled, ruleData.enabled) && Objects.equals(loged, ruleData.loged)
                && Objects.equals(handle, ruleData.handle) && Objects.equals(conditionDataList, ruleData.conditionDataList);
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

        /**
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return RuleData
         */
        public RuleData build() {
            return new RuleData(this);
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
    }
}
