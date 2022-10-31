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

import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.hibernate.validator.constraints.Range;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * this is selector from by web front.
 */
public final class SelectorDTO implements Serializable {
    
    private static final long serialVersionUID = -4979839188302702999L;
    
    /**
     * primary key.
     */
    @Existed(provider = SelectorMapper.class, nullOfIgnore = true, message = "selector is not existed")
    private String id;
    
    /**
     * plugin id.
     */
    @NotBlank
    @Existed(provider = PluginMapper.class, message = "plugin is not existed")
    private String pluginId;
    
    /**
     * selector name.
     */
    @NotBlank
    private String name;
    
    /**
     * match mode. 0 and 1 or
     */
    private Integer matchMode;
    
    /**
     * selector type.
     */
    @NotNull
    @Min(value = 0)
    @Max(value = 1)
    private Integer type;
    
    /**
     * sort type.
     */
    @NotNull
    @Range(min = 1, max = 1000)
    private Integer sort;
    
    /**
     * whether enabled.
     */
    @NotNull
    private Boolean enabled;
    
    /**
     * whether loged.
     */
    @NotNull
    private Boolean loged;
    
    /**
     * whether continued.
     */
    @NotNull
    private Boolean continued;
    
    /**
     * handle.
     */
    private String handle;
    
    /**
     * selector conditions.
     */
    private List<SelectorConditionDTO> selectorConditions;
    
    public SelectorDTO() {
    }
    
    public SelectorDTO(final String id,
                       @NotBlank final String pluginId,
                       @NotBlank final String name,
                       final Integer matchMode,
                       @NotNull final Integer type,
                       @NotNull final Integer sort,
                       @NotNull final Boolean enabled,
                       final Boolean loged,
                       @NotNull final Boolean continued,
                       final String handle,
                       @Valid final List<SelectorConditionDTO> selectorConditions) {
        this.id = id;
        this.pluginId = pluginId;
        this.name = name;
        this.matchMode = matchMode;
        this.type = type;
        this.sort = sort;
        this.enabled = enabled;
        this.loged = loged;
        this.continued = continued;
        this.handle = handle;
        this.selectorConditions = selectorConditions;
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
    public List<SelectorConditionDTO> getSelectorConditions() {
        return selectorConditions;
    }
    
    /**
     * Sets the selectorConditions.
     *
     * @param selectorConditions selectorConditions
     */
    public void setSelectorConditions(final List<SelectorConditionDTO> selectorConditions) {
        this.selectorConditions = selectorConditions;
    }
    
    /**
     * builder method.
     *
     * @return builder object.
     */
    public static SelectorDTO.SelectorDTOBuilder builder() {
        return new SelectorDTO.SelectorDTOBuilder();
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof SelectorDTO)) {
            return false;
        }
        SelectorDTO that = (SelectorDTO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(pluginId, that.pluginId)
                && Objects.equals(name, that.name)
                && Objects.equals(matchMode, that.matchMode)
                && Objects.equals(type, that.type)
                && Objects.equals(sort, that.sort)
                && Objects.equals(enabled, that.enabled)
                && Objects.equals(loged, that.loged)
                && Objects.equals(continued, that.continued)
                && Objects.equals(handle, that.handle)
                && Objects.equals(selectorConditions, that.selectorConditions);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(id, pluginId, name, matchMode, type, sort, enabled, loged, continued, handle, selectorConditions);
    }
    
    public static final class SelectorDTOBuilder {
        
        private String id;
        
        private String pluginId;
        
        private String name;
        
        private Integer matchMode;
        
        private Integer type;
        
        private Integer sort;
        
        private Boolean enabled;
        
        private Boolean loged;
        
        private Boolean continued;
        
        private String handle;
        
        private List<SelectorConditionDTO> selectorConditions;
        
        private SelectorDTOBuilder() {
        }
        
        /**
         * id.
         *
         * @param id the id.
         * @return SelectorDTOBuilder.
         */
        public SelectorDTOBuilder id(final String id) {
            this.id = id;
            return this;
        }
        
        /**
         * pluginId.
         *
         * @param pluginId the pluginId.
         * @return SelectorDTOBuilder.
         */
        public SelectorDTOBuilder pluginId(final String pluginId) {
            this.pluginId = pluginId;
            return this;
        }
        
        /**
         * name.
         *
         * @param name the name.
         * @return SelectorDTOBuilder.
         */
        public SelectorDTOBuilder name(final String name) {
            this.name = name;
            return this;
        }
        
        /**
         * matchMode.
         *
         * @param matchMode the matchMode.
         * @return SelectorDTOBuilder.
         */
        public SelectorDTOBuilder matchMode(final Integer matchMode) {
            this.matchMode = matchMode;
            return this;
        }
        
        /**
         * type.
         *
         * @param type the type.
         * @return SelectorDTOBuilder.
         */
        public SelectorDTOBuilder type(final Integer type) {
            this.type = type;
            return this;
        }
        
        /**
         * sort.
         *
         * @param sort the sort.
         * @return SelectorDTOBuilder.
         */
        public SelectorDTOBuilder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }
        
        /**
         * enabled.
         *
         * @param enabled the enabled.
         * @return SelectorDTOBuilder.
         */
        public SelectorDTOBuilder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }
        
        /**
         * loged.
         *
         * @param loged the loged.
         * @return SelectorDTOBuilder.
         */
        public SelectorDTOBuilder loged(final Boolean loged) {
            this.loged = loged;
            return this;
        }
        
        /**
         * continued.
         *
         * @param continued the continued.
         * @return SelectorDTOBuilder.
         */
        public SelectorDTOBuilder continued(final Boolean continued) {
            this.continued = continued;
            return this;
        }
        
        /**
         * handle.
         *
         * @param handle the handle.
         * @return SelectorDTOBuilder.
         */
        public SelectorDTOBuilder handle(final String handle) {
            this.handle = handle;
            return this;
        }
        
        /**
         * selectorConditions.
         *
         * @param selectorConditions the selectorConditions.
         * @return SelectorDTOBuilder.
         */
        public SelectorDTOBuilder selectorConditions(final List<SelectorConditionDTO> selectorConditions) {
            this.selectorConditions = selectorConditions;
            return this;
        }
        
        /**
         * build method.
         *
         * @return build object.
         */
        public SelectorDTO build() {
            return new SelectorDTO(id, pluginId, name, matchMode, type, sort, enabled, loged, continued, handle, selectorConditions);
        }
    }
}
