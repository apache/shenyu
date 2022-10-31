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
import org.apache.shenyu.admin.model.dto.PluginHandleDTO;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.Objects;
import java.util.Optional;

/**
 * plugin handle json definition.
 */
public final class PluginHandleDO extends BaseDO {

    private static final long serialVersionUID = 3854807942396454551L;

    /**
     * plugin id.
     */
    private String pluginId;

    /**
     * the attribute name.
     */
    private String field;

    /**
     * the attribute label.
     */
    private String label;

    /**
     * the data type.
     * 1 indicates number
     * 2 indicates string
     * 3 indicates select box.
     */
    private Integer dataType;

    /**
     *  the attribute type.
     *  1  selector,
     *  2  rule.
     */
    private Integer type;

    /**
     * the attribute sort.
     */
    private Integer sort;

    /**
     * the attribute extObj.
     */
    private String extObj;

    public PluginHandleDO() {
    }

    public PluginHandleDO(final String pluginId, final String field, final String label, final Integer dataType, final Integer type, final Integer sort, final String extObj) {
        this.pluginId = pluginId;
        this.field = field;
        this.label = label;
        this.dataType = dataType;
        this.type = type;
        this.sort = sort;
        this.extObj = extObj;
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
     * Gets the value of field.
     *
     * @return the value of field
     */
    public String getField() {
        return field;
    }

    /**
     * Sets the field.
     *
     * @param field field
     */
    public void setField(final String field) {
        this.field = field;
    }

    /**
     * Gets the value of label.
     *
     * @return the value of label
     */
    public String getLabel() {
        return label;
    }

    /**
     * Sets the label.
     *
     * @param label label
     */
    public void setLabel(final String label) {
        this.label = label;
    }

    /**
     * Gets the value of dataType.
     *
     * @return the value of dataType
     */
    public Integer getDataType() {
        return dataType;
    }

    /**
     * Sets the dataType.
     *
     * @param dataType dataType
     */
    public void setDataType(final Integer dataType) {
        this.dataType = dataType;
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
     * Gets the value of extObj.
     *
     * @return the value of extObj
     */
    public String getExtObj() {
        return extObj;
    }

    /**
     * Sets the extObj.
     *
     * @param extObj extObj
     */
    public void setExtObj(final String extObj) {
        this.extObj = extObj;
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static PluginHandleDO.PluginHandleDOBuilder builder() {
        return new PluginHandleDO.PluginHandleDOBuilder();
    }

    /**
     * build {@linkplain PluginHandleDO} instance.
     * @param pluginHandleDTO {@linkplain PluginHandleDTO}
     * @return {@linkplain PluginHandleDO}
     */
    public static PluginHandleDO buildPluginHandleDO(final PluginHandleDTO pluginHandleDTO) {
        return Optional.ofNullable(pluginHandleDTO).map(item -> {
            PluginHandleDO pluginHandleDO = PluginHandleDO.builder()
                    .id(item.getId())
                    .pluginId(item.getPluginId())
                    .field(item.getField())
                    .label(item.getLabel())
                    .dataType(item.getDataType())
                    .type(item.getType())
                    .sort(item.getSort())
                    .extObj(item.getExtObj())
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                pluginHandleDO.setId(UUIDUtils.getInstance().generateShortUuid());
            }
            return pluginHandleDO;
        }).orElse(null);
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
        PluginHandleDO that = (PluginHandleDO) o;
        return Objects.equals(pluginId, that.pluginId)
                && Objects.equals(field, that.field)
                && Objects.equals(label, that.label)
                && Objects.equals(dataType, that.dataType)
                && Objects.equals(type, that.type)
                && Objects.equals(sort, that.sort)
                && Objects.equals(extObj, that.extObj);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), pluginId, field, label, dataType, type, sort, extObj);
    }

    public static final class PluginHandleDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String pluginId;

        private String field;

        private String label;

        private Integer dataType;

        private Integer type;

        private Integer sort;

        private String extObj;

        private PluginHandleDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return PluginHandleDOBuilder.
         */
        public PluginHandleDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return PluginHandleDOBuilder.
         */
        public PluginHandleDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return PluginHandleDOBuilder.
         */
        public PluginHandleDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * pluginId.
         *
         * @param pluginId the pluginId.
         * @return PluginHandleDOBuilder.
         */
        public PluginHandleDOBuilder pluginId(final String pluginId) {
            this.pluginId = pluginId;
            return this;
        }

        /**
         * field.
         *
         * @param field the field.
         * @return PluginHandleDOBuilder.
         */
        public PluginHandleDOBuilder field(final String field) {
            this.field = field;
            return this;
        }

        /**
         * label.
         *
         * @param label the label.
         * @return PluginHandleDOBuilder.
         */
        public PluginHandleDOBuilder label(final String label) {
            this.label = label;
            return this;
        }

        /**
         * dataType.
         *
         * @param dataType the dataType.
         * @return PluginHandleDOBuilder.
         */
        public PluginHandleDOBuilder dataType(final Integer dataType) {
            this.dataType = dataType;
            return this;
        }

        /**
         * type.
         *
         * @param type the type.
         * @return PluginHandleDOBuilder.
         */
        public PluginHandleDOBuilder type(final Integer type) {
            this.type = type;
            return this;
        }

        /**
         * sort.
         *
         * @param sort the sort.
         * @return PluginHandleDOBuilder.
         */
        public PluginHandleDOBuilder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }

        /**
         * extObj.
         *
         * @param extObj the extObj.
         * @return PluginHandleDOBuilder.
         */
        public PluginHandleDOBuilder extObj(final String extObj) {
            this.extObj = extObj;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public PluginHandleDO build() {
            PluginHandleDO pluginHandleDO = new PluginHandleDO();
            pluginHandleDO.setId(id);
            pluginHandleDO.setDateCreated(dateCreated);
            pluginHandleDO.setDateUpdated(dateUpdated);
            pluginHandleDO.setPluginId(pluginId);
            pluginHandleDO.setField(field);
            pluginHandleDO.setLabel(label);
            pluginHandleDO.setDataType(dataType);
            pluginHandleDO.setType(type);
            pluginHandleDO.setSort(sort);
            pluginHandleDO.setExtObj(extObj);
            return pluginHandleDO;
        }
    }
}
