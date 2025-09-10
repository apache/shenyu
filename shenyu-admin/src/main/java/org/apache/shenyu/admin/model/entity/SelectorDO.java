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
import java.util.Collections;
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
    private String selectorName;

    /**
     * match mode.
     */
    private Integer matchMode;

    /**
     * selector type.
     */
    private Integer selectorType;

    /**
     * sort code.
     */
    private Integer sortCode;

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

    /**
     * namespaceId.
     */
    private String namespaceId;

    public SelectorDO() {
    }

    public SelectorDO(final String pluginId,
                      final String selectorName,
                      final Integer matchMode,
                      final Integer selectorType,
                      final Integer sortCode,
                      final Boolean enabled,
                      final Boolean loged,
                      final Boolean continued,
                      final String handle,
                      final Boolean matchRestful,
                      final String namespaceId) {
        this.pluginId = pluginId;
        this.selectorName = selectorName;
        this.matchMode = matchMode;
        this.selectorType = selectorType;
        this.sortCode = sortCode;
        this.enabled = enabled;
        this.loged = loged;
        this.continued = continued;
        this.handle = handle;
        this.matchRestful = matchRestful;
        this.namespaceId = namespaceId;
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
     * Gets the value of selectorName.
     *
     * @return the value of selectorName
     */
    public String getSelectorName() {
        return selectorName;
    }

    /**
     * Sets the selectorName.
     *
     * @param selectorName selectorName
     */
    public void setSelectorName(final String selectorName) {
        this.selectorName = selectorName;
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
     * Gets the value of selectorType.
     *
     * @return the value of selectorType
     */
    public Integer getSelectorType() {
        return selectorType;
    }

    /**
     * Sets the selectorType.
     *
     * @param selectorType selectorType
     */
    public void setSelectorType(final Integer selectorType) {
        this.selectorType = selectorType;
    }

    /**
     * Gets the value of sortCode.
     *
     * @return the value of sortCode
     */
    public Integer getSortCode() {
        return sortCode;
    }

    /**
     * Sets the sortCode.
     *
     * @param sortCode sortCode
     */
    public void setSortCode(final Integer sortCode) {
        this.sortCode = sortCode;
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
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        SelectorDO that = (SelectorDO) o;
        return Objects.equals(pluginId, that.pluginId)
                && Objects.equals(selectorName, that.selectorName)
                && Objects.equals(matchMode, that.matchMode)
                && Objects.equals(selectorType, that.selectorType)
                && Objects.equals(sortCode, that.sortCode)
                && Objects.equals(enabled, that.enabled)
                && Objects.equals(loged, that.loged)
                && Objects.equals(continued, that.continued)
                && Objects.equals(handle, that.handle)
                && Objects.equals(matchRestful, that.matchRestful)
                && Objects.equals(namespaceId, that.namespaceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), pluginId, selectorName, matchMode, selectorType, sortCode, enabled, loged, continued, handle, matchRestful, namespaceId);
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
                    .selectorType(item.getType())
                    .sortCode(item.getSort())
                    .enabled(item.getEnabled())
                    .loged(item.getLoged())
                    .continued(item.getContinued())
                    .dateUpdated(currentTime)
                    .handle(item.getHandle())
                    .pluginId(item.getPluginId())
                    .selectorName(item.getName())
                    .matchRestful(item.getMatchRestful())
                    .namespaceId(item.getNamespaceId())
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
     * @param selectorDO              the selector do
     * @param pluginName              the plugin name
     * @param conditionDataList       the condition data list
     * @param beforeConditionDataList before condition data list
     * @return the selector data
     */
    public static SelectorData transFrom(final SelectorDO selectorDO, final String pluginName,
                                         final List<ConditionData> conditionDataList, final List<ConditionData> beforeConditionDataList) {
        return SelectorData.builder()
                .id(selectorDO.getId())
                .pluginId(selectorDO.getPluginId())
                .pluginName(pluginName)
                .name(selectorDO.getSelectorName())
                .matchMode(selectorDO.getMatchMode())
                .type(selectorDO.getSelectorType())
                .sort(selectorDO.getSortCode())
                .enabled(selectorDO.getEnabled())
                .logged(selectorDO.getLoged())
                .continued(selectorDO.getContinued())
                .handle(selectorDO.getHandle())
                .conditionList(conditionDataList)
                .matchRestful(selectorDO.getMatchRestful())
                .beforeConditionList(beforeConditionDataList)
                .namespaceId(selectorDO.getNamespaceId())
                .build();
    }

    /**
     * selector data transform.
     *
     * @param selectorDO selector entity
     * @param pluginName plugin name
     * @param conditionDataList condition data list
     * @return the selector data
     */
    public static SelectorData transFrom(final SelectorDO selectorDO, final String pluginName, final List<ConditionData> conditionDataList) {
        return transFrom(selectorDO, pluginName, conditionDataList, Collections.emptyList());
    }

    public static final class SelectorDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String pluginId;

        private String selectorName;

        private Integer matchMode;

        private Integer selectorType;

        private Integer sortCode;

        private Boolean enabled;

        private Boolean loged;

        private Boolean continued;

        private String handle;

        private Boolean matchRestful;

        private String namespaceId;

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
         * selectorName.
         *
         * @param selectorName the selectorName.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder selectorName(final String selectorName) {
            this.selectorName = selectorName;
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
         * @param selectorType the type.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder selectorType(final Integer selectorType) {
            this.selectorType = selectorType;
            return this;
        }

        /**
         * sortCode.
         *
         * @param sortCode the sortCode.
         * @return SelectorDOBuilder.
         */
        public SelectorDOBuilder sortCode(final Integer sortCode) {
            this.sortCode = sortCode;
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
         * namespaceId.
         *
         * @param namespaceId namespaceId
         * @return SelectorDOBuilder
         */
        public SelectorDOBuilder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
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
            selectorDO.setSelectorName(selectorName);
            selectorDO.setMatchMode(matchMode);
            selectorDO.setSelectorType(selectorType);
            selectorDO.setSortCode(sortCode);
            selectorDO.setEnabled(enabled);
            selectorDO.setLoged(loged);
            selectorDO.setContinued(continued);
            selectorDO.setHandle(handle);
            selectorDO.setMatchRestful(matchRestful);
            selectorDO.setNamespaceId(namespaceId);
            return selectorDO;
        }
    }
}
