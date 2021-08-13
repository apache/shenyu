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

package org.apache.shenyu.admin.model.dto;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * this is rule from by web front.
 */
public final class RuleDTO implements Serializable {

    private static final long serialVersionUID = 995629439944393704L;

    /**
     * primary key.
     */
    private String id;

    /**
     * selector id.
     */
    @NotNull
    private String selectorId;

    /**
     * match mode.
     */
    @NotNull
    private Integer matchMode;

    /**
     * rule name.
     */
    @NotNull
    private String name;

    /**
     * whether enabled.
     */
    @NotNull
    private Boolean enabled;

    /**
     * whether loged.
     */
    private Boolean loged;

    /**
     * sort type.
     */
    @NotNull
    private Integer sort;

    /**
     * process logic.
     */
    private String handle;

    /**
     * rule conditions.
     */
    @Valid
    private List<RuleConditionDTO> ruleConditions;

    public RuleDTO() {
    }

    public RuleDTO(final String id,
                   @NotNull final String selectorId,
                   @NotNull final Integer matchMode,
                   @NotNull final String name,
                   @NotNull final Boolean enabled,
                   final Boolean loged,
                   @NotNull final Integer sort,
                   final String handle,
                   @Valid final List<RuleConditionDTO> ruleConditions) {
        this.id = id;
        this.selectorId = selectorId;
        this.matchMode = matchMode;
        this.name = name;
        this.enabled = enabled;
        this.loged = loged;
        this.sort = sort;
        this.handle = handle;
        this.ruleConditions = ruleConditions;
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
     * Gets the value of ruleConditions.
     *
     * @return the value of ruleConditions
     */
    public List<RuleConditionDTO> getRuleConditions() {
        return ruleConditions;
    }

    /**
     * Sets the ruleConditions.
     *
     * @param ruleConditions ruleConditions
     */
    public void setRuleConditions(final List<RuleConditionDTO> ruleConditions) {
        this.ruleConditions = ruleConditions;
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static RuleDTO.RuleDTOBuilder builder() {
        return new RuleDTO.RuleDTOBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof RuleDTO)) {
            return false;
        }
        RuleDTO ruleDTO = (RuleDTO) o;
        return Objects.equals(id, ruleDTO.id)
                && Objects.equals(selectorId, ruleDTO.selectorId)
                && Objects.equals(matchMode, ruleDTO.matchMode)
                && Objects.equals(name, ruleDTO.name)
                && Objects.equals(enabled, ruleDTO.enabled)
                && Objects.equals(loged, ruleDTO.loged)
                && Objects.equals(sort, ruleDTO.sort)
                && Objects.equals(handle, ruleDTO.handle)
                && Objects.equals(ruleConditions, ruleDTO.ruleConditions);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, selectorId, matchMode, name, enabled, loged, sort, handle, ruleConditions);
    }

    public static final class RuleDTOBuilder {

        private String id;

        private String selectorId;

        private Integer matchMode;

        private String name;

        private Boolean enabled;

        private Boolean loged;

        private Integer sort;

        private String handle;

        private List<RuleConditionDTO> ruleConditions;

        private RuleDTOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return RuleDTOBuilder.
         */
        public RuleDTOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * selectorId.
         *
         * @param selectorId the selectorId.
         * @return RuleDTOBuilder.
         */
        public RuleDTOBuilder selectorId(final String selectorId) {
            this.selectorId = selectorId;
            return this;
        }

        /**
         * matchMode.
         *
         * @param matchMode the matchMode.
         * @return RuleDTOBuilder.
         */
        public RuleDTOBuilder matchMode(final Integer matchMode) {
            this.matchMode = matchMode;
            return this;
        }

        /**
         * name.
         *
         * @param name the name.
         * @return RuleDTOBuilder.
         */
        public RuleDTOBuilder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * enabled.
         *
         * @param enabled the enabled.
         * @return RuleDTOBuilder.
         */
        public RuleDTOBuilder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * loged.
         *
         * @param loged the loged.
         * @return RuleDTOBuilder.
         */
        public RuleDTOBuilder loged(final Boolean loged) {
            this.loged = loged;
            return this;
        }

        /**
         * sort.
         *
         * @param sort the sort.
         * @return RuleDTOBuilder.
         */
        public RuleDTOBuilder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }

        /**
         * handle.
         *
         * @param handle the handle.
         * @return RuleDTOBuilder.
         */
        public RuleDTOBuilder handle(final String handle) {
            this.handle = handle;
            return this;
        }

        /**
         * ruleConditions.
         *
         * @param ruleConditions the ruleConditions.
         * @return RuleDTOBuilder.
         */
        public RuleDTOBuilder ruleConditions(final List<RuleConditionDTO> ruleConditions) {
            this.ruleConditions = ruleConditions;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public RuleDTO build() {
            return new RuleDTO(id, selectorId, matchMode, name, enabled, loged, sort, handle, ruleConditions);
        }
    }
}
