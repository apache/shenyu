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
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * SelectorDO.
 */
public final class SelectorDO extends BaseDO {

    private static final long serialVersionUID = -1627940797162331235L;

    /**
     * plugin id.
     */
    private String pluginId;

    /**
     * selector name.
     */
    private String name;

    /**
     * match mode.
     */
    private Integer matchMode;

    /**
     * selector type.
     */
    private Integer type;

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

    /**
     * handle.
     */
    private String handle;
    
    /**
     * match restful.
     */
    private Boolean matchRestful;

    public SelectorDO() {
    }

    public SelectorDO(final String pluginId,
                      final String name,
                      final Integer matchMode,
                      final Integer type,
                      final Integer sort,
                      final Boolean enabled,
                      final Boolean loged,
                      final Boolean continued,
                      final String handle,
                      final Boolean matchRestful) {
        this.pluginId = pluginId;
        this.name = name;
        this.matchMode = matchMode;
        this.type = type;
        this.sort = sort;
        this.enabled = enabled;
        this.loged = loged;
        this.continued = continued;
        this.handle = handle;
        this.matchRestful = matchRestful;
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
     * builder method.
     *
     * @return builder object.
     */
    public static SelectorDO.SelectorDOBuilder builder() {
        return new SelectorDO.SelectorDOBuilder();
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
        SelectorDO that = (SelectorDO) o;
        return Objects.equals(pluginId, that.pluginId)
                && Objects.equals(name, that.name)
                && Objects.equals(matchMode, that.matchMode)
                && Objects.equals(type, that.type)
                && Objects.equals(sort, that.sort)
                && Objects.equals(enabled, that.enabled)
                && Objects.equals(loged, that.loged)
                && Objects.equals(continued, that.continued)
                && Objects.equals(handle, that.handle)
                && Objects.equals(matchRestful, that.matchRestful);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), pluginId, name, matchMode, type, sort, enabled, loged, continued, handle, matchRestful);
    }

    /**
     * build selectorDO.
     *
     * @param selectorDTO {@linkplain SelectorDTO}
     * @return {@linkplain SelectorDO}
     */
    public static SelectorDO buildSelectorDO(final SelectorDTO selectorDTO) {
        return Optional.ofNullable(selectorDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            SelectorDO selectorDO = SelectorDO.builder()
                    .type(item.getType())
                    .sort(item.getSort())
                    .enabled(item.getEnabled())
                    .loged(item.getLoged())
                    .continued(item.getContinued())
                    .dateUpdated(currentTime)
                    .handle(item.getHandle())
                    .pluginId(item.getPluginId())
                    .name(item.getName())
                    .matchRestful(item.getMatchRestful())
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                selectorDO.setId(UUIDUtils.getInstance().generateShortUuid());
                selectorDO.setDateCreated(currentTime);
            } else {
                selectorDO.setId(item.getId());
            }
            if (SelectorTypeEnum.FULL_FLOW.getCode() == item.getType()) {
                selectorDO.setMatchMode(MatchModeEnum.AND.getCode());
            } else {
                selectorDO.setMatchMode(item.getMatchMode());
            }
            return selectorDO;
        }).orElse(null);
    }

    /**
     * Trans from selector data.
     *
     * @param selectorDO        the selector do
     * @param pluginName        the plugin name
     * @param conditionDataList the condition data list
     * @return the selector data
     */
    public static SelectorData transFrom(final SelectorDO selectorDO, final String pluginName, final List<ConditionData> conditionDataList) {
        return SelectorData.builder()
                .id(selectorDO.getId())
                .pluginId(selectorDO.getPluginId())
                .pluginName(pluginName)
                .name(selectorDO.getName())
                .matchMode(selectorDO.getMatchMode())
                .type(selectorDO.getType())
                .sort(selectorDO.getSort())
                .enabled(selectorDO.getEnabled())
                .logged(selectorDO.getLoged())
                .continued(selectorDO.getContinued())
                .handle(selectorDO.getHandle())
                .conditionList(conditionDataList)
                .matchRestful(selectorDO.getMatchRestful())
                .build();
    }

    public static final class SelectorDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String pluginId;

        private String name;

        private Integer matchMode;

        private Integer type;

        private Integer sort;

        private Boolean enabled;

        private Boolean loged;

        private Boolean continued;

        private String handle;
        
        private Boolean matchRestful;

        private SelectorDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * pluginId.
         *
         * @param pluginId the pluginId.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder pluginId(final String pluginId) {
            this.pluginId = pluginId;
            return this;
        }

        /**
         * name.
         *
         * @param name the name.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * matchMode.
         *
         * @param matchMode the matchMode.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder matchMode(final Integer matchMode) {
            this.matchMode = matchMode;
            return this;
        }

        /**
         * type.
         *
         * @param type the type.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder type(final Integer type) {
            this.type = type;
            return this;
        }

        /**
         * sort.
         *
         * @param sort the sort.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }

        /**
         * enabled.
         *
         * @param enabled the enabled.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * loged.
         *
         * @param loged the loged.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder loged(final Boolean loged) {
            this.loged = loged;
            return this;
        }

        /**
         * continued.
         *
         * @param continued the continued.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder continued(final Boolean continued) {
            this.continued = continued;
            return this;
        }

        /**
         * handle.
         *
         * @param handle the handle.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder handle(final String handle) {
            this.handle = handle;
            return this;
        }
    
        /**
         * match restful.
         *
         * @param matchRestful matchRestful
         * @return SelectorDOBuilder
         */
        public SelectorDOBuilder matchRestful(final Boolean matchRestful) {
            this.matchRestful = matchRestful;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public SelectorDO build() {
            SelectorDO selectorDO = new SelectorDO();
            selectorDO.setId(id);
            selectorDO.setDateCreated(dateCreated);
            selectorDO.setDateUpdated(dateUpdated);
            selectorDO.setPluginId(pluginId);
            selectorDO.setName(name);
            selectorDO.setMatchMode(matchMode);
            selectorDO.setType(type);
            selectorDO.setSort(sort);
            selectorDO.setEnabled(enabled);
            selectorDO.setLoged(loged);
            selectorDO.setContinued(continued);
            selectorDO.setHandle(handle);
            selectorDO.setMatchRestful(matchRestful);
            return selectorDO;
        }
    }
}
