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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * RuleDO.
 */
public final class RuleDO extends BaseDO {

    private static final long serialVersionUID = 8050178277098166539L;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * match mode.
     */
    private Integer matchMode;

    /**
     * rule name.
     */
    private String name;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * whether loged.
     */
    private Boolean loged;

    /**
     * sort type.
     */
    private Integer sort;

    /**
     * process logic.
     */
    private String handle;
    
    /**
     * match restful.
     */
    private Boolean matchRestful;

    public RuleDO() {
    }

    public RuleDO(final String selectorId,
                  final Integer matchMode,
                  final String name,
                  final Boolean enabled,
                  final Boolean loged,
                  final Integer sort,
                  final String handle,
                  final Boolean matchRestful) {
        this.selectorId = selectorId;
        this.matchMode = matchMode;
        this.name = name;
        this.enabled = enabled;
        this.loged = loged;
        this.sort = sort;
        this.handle = handle;
        this.matchRestful = matchRestful;
    }

    /**
     * Gets the value of selectorId.
     *
     * @return the value of selectorId
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * Sets the selectorId.
     *
     * @param selectorId selectorId
     */
    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
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
     * get match restful uri.
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
     * builder method.
     *
     * @return builder object.
     */
    public static RuleDO.RuleDOBuilder builder() {
        return new RuleDO.RuleDOBuilder();
    }

    /**
     * build ruleDO.
     *
     * @param ruleDTO {@linkplain RuleDTO}
     * @return {@linkplain RuleDO}
     */
    public static RuleDO buildRuleDO(final RuleDTO ruleDTO) {
        return Optional.ofNullable(ruleDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            RuleDO ruleDO = RuleDO.builder()
                    .selectorId(item.getSelectorId())
                    .matchMode(item.getMatchMode())
                    .name(item.getName())
                    .enabled(item.getEnabled())
                    .loged(item.getLoged())
                    .sort(item.getSort())
                    .handle(item.getHandle())
                    .matchRestful(item.getMatchRestful())
                    .dateUpdated(currentTime)
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                ruleDO.setId(UUIDUtils.getInstance().generateShortUuid());
                ruleDO.setDateCreated(currentTime);
            } else {
                ruleDO.setId(item.getId());
            }
            return ruleDO;
        }).orElse(null);
    }

    /**
     * Trans from rule data.
     *
     * @param ruleDO            the rule do
     * @param pluginName        the plugin name
     * @param conditionDataList the condition data list
     * @param beforeConditionDataList the before condition data list
     * @return the rule data
     */
    public static RuleData transFrom(final RuleDO ruleDO, final String pluginName, final List<ConditionData> conditionDataList, final List<ConditionData> beforeConditionDataList) {
        return RuleData.builder()
                .id(ruleDO.getId())
                .name(ruleDO.getName())
                .pluginName(pluginName)
                .selectorId(ruleDO.getSelectorId())
                .matchMode(ruleDO.getMatchMode())
                .sort(ruleDO.getSort())
                .enabled(ruleDO.getEnabled())
                .loged(ruleDO.getLoged())
                .handle(ruleDO.getHandle())
                .matchRestful(ruleDO.getMatchRestful())
                .conditionDataList(conditionDataList)
                .beforeConditionDataList(beforeConditionDataList)
                .build();
    }

    /**
     * Trans from rule data.
     *
     * @param ruleDO            the rule do
     * @param pluginName        the plugin name
     * @param conditionDataList the condition data list
     *
     * @return ruleData
     */
    public static RuleData transFrom(final RuleDO ruleDO, final String pluginName, final List<ConditionData> conditionDataList) {
        return transFrom(ruleDO, pluginName, conditionDataList, Collections.emptyList());
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        RuleDO ruleDO = (RuleDO) o;
        return Objects.equals(selectorId, ruleDO.selectorId)
                && Objects.equals(matchMode, ruleDO.matchMode)
                && Objects.equals(name, ruleDO.name)
                && Objects.equals(enabled, ruleDO.enabled)
                && Objects.equals(loged, ruleDO.loged)
                && Objects.equals(sort, ruleDO.sort)
                && Objects.equals(handle, ruleDO.handle)
                && Objects.equals(matchRestful, ruleDO.matchRestful);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), selectorId, matchMode, name, enabled, loged, sort, handle, matchRestful);
    }

    public static final class RuleDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String selectorId;

        private Integer matchMode;

        private String name;

        private Boolean enabled;

        private Boolean loged;

        private Integer sort;

        private String handle;
        
        private Boolean matchRestful;

        private RuleDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return RuleDOBuilder.
         */
        public RuleDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return RuleDOBuilder.
         */
        public RuleDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return RuleDOBuilder.
         */
        public RuleDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * selectorId.
         *
         * @param selectorId the selectorId.
         * @return RuleDOBuilder.
         */
        public RuleDOBuilder selectorId(final String selectorId) {
            this.selectorId = selectorId;
            return this;
        }

        /**
         * matchMode.
         *
         * @param matchMode the matchMode.
         * @return RuleDOBuilder.
         */
        public RuleDOBuilder matchMode(final Integer matchMode) {
            this.matchMode = matchMode;
            return this;
        }

        /**
         * name.
         *
         * @param name the name.
         * @return RuleDOBuilder.
         */
        public RuleDOBuilder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * enabled.
         *
         * @param enabled the enabled.
         * @return RuleDOBuilder.
         */
        public RuleDOBuilder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * loged.
         *
         * @param loged the loged.
         * @return RuleDOBuilder.
         */
        public RuleDOBuilder loged(final Boolean loged) {
            this.loged = loged;
            return this;
        }

        /**
         * sort.
         *
         * @param sort the sort.
         * @return RuleDOBuilder.
         */
        public RuleDOBuilder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }

        /**
         * handle.
         *
         * @param handle the handle.
         * @return RuleDOBuilder.
         */
        public RuleDOBuilder handle(final String handle) {
            this.handle = handle;
            return this;
        }
    
        /**
         * matchRestful.
         *
         * @param matchRestful matchRestful
         * @return RuleDOBuilder
         */
        public RuleDOBuilder matchRestful(final Boolean matchRestful) {
            this.matchRestful = matchRestful;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public RuleDO build() {
            RuleDO ruleDO = new RuleDO();
            ruleDO.setId(id);
            ruleDO.setDateCreated(dateCreated);
            ruleDO.setDateUpdated(dateUpdated);
            ruleDO.setSelectorId(selectorId);
            ruleDO.setMatchMode(matchMode);
            ruleDO.setName(name);
            ruleDO.setEnabled(enabled);
            ruleDO.setLoged(loged);
            ruleDO.setSort(sort);
            ruleDO.setHandle(handle);
            ruleDO.setMatchRestful(matchRestful);
            return ruleDO;
        }
    }
}
