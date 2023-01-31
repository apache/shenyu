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
import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.Objects;
import java.util.Optional;

/**
 * ShenyuDictDO.
 */
public final class ShenyuDictDO extends BaseDO {

    private static final long serialVersionUID = -3968123108441795604L;

    /**
     * dict type.
     */
    private String type;

    /**
     * dict code.
     */
    private String dictCode;

    /**
     * dict name.
     */
    private String dictName;

    /**
     * dict value.
     */
    private String dictValue;

    /**
     * dict desc.
     */
    private String desc;

    /**
     * sort no.
     */
    private Integer sort;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    public ShenyuDictDO() {
    }

    public ShenyuDictDO(final String type, final String dictCode, final String dictName, final String dictValue, final String desc, final Integer sort, final Boolean enabled) {
        this.type = type;
        this.dictCode = dictCode;
        this.dictName = dictName;
        this.dictValue = dictValue;
        this.desc = desc;
        this.sort = sort;
        this.enabled = enabled;
    }

    /**
     * Gets the value of type.
     *
     * @return the value of type
     */
    public String getType() {
        return type;
    }

    /**
     * Sets the type.
     *
     * @param type type
     */
    public void setType(final String type) {
        this.type = type;
    }

    /**
     * Gets the value of dictCode.
     *
     * @return the value of dictCode
     */
    public String getDictCode() {
        return dictCode;
    }

    /**
     * Sets the dictCode.
     *
     * @param dictCode dictCode
     */
    public void setDictCode(final String dictCode) {
        this.dictCode = dictCode;
    }

    /**
     * Gets the value of dictName.
     *
     * @return the value of dictName
     */
    public String getDictName() {
        return dictName;
    }

    /**
     * Sets the dictName.
     *
     * @param dictName dictName
     */
    public void setDictName(final String dictName) {
        this.dictName = dictName;
    }

    /**
     * Gets the value of dictValue.
     *
     * @return the value of dictValue
     */
    public String getDictValue() {
        return dictValue;
    }

    /**
     * Sets the dictValue.
     *
     * @param dictValue dictValue
     */
    public void setDictValue(final String dictValue) {
        this.dictValue = dictValue;
    }

    /**
     * Gets the value of desc.
     *
     * @return the value of desc
     */
    public String getDesc() {
        return desc;
    }

    /**
     * Sets the desc.
     *
     * @param desc desc
     */
    public void setDesc(final String desc) {
        this.desc = desc;
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
     * builder method.
     *
     * @return builder object.
     */
    public static ShenyuDictDO.ShenyuDictDOBuilder builder() {
        return new ShenyuDictDO.ShenyuDictDOBuilder();
    }

    /**
     * build {@linkplain ShenyuDictDO} instance.
     *
     * @param shenyuDictDTO {@linkplain ShenyuDictDTO}
     * @return {@linkplain ShenyuDictDO}
     */
    public static ShenyuDictDO buildShenyuDictDO(final ShenyuDictDTO shenyuDictDTO) {
        return Optional.ofNullable(shenyuDictDTO).map(item -> {
            ShenyuDictDO shenyuDictDO = ShenyuDictDO.builder()
                    .id(item.getId())
                    .dictCode(item.getDictCode())
                    .dictName(item.getDictName())
                    .dictValue(item.getDictValue())
                    .desc(item.getDesc())
                    .enabled(item.getEnabled())
                    .sort(item.getSort())
                    .type(item.getType())
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                shenyuDictDO.setId(UUIDUtils.getInstance().generateShortUuid());
            }
            return shenyuDictDO;
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
        ShenyuDictDO that = (ShenyuDictDO) o;
        return Objects.equals(type, that.type)
                && Objects.equals(dictCode, that.dictCode)
                && Objects.equals(dictName, that.dictName)
                && Objects.equals(dictValue, that.dictValue)
                && Objects.equals(desc, that.desc)
                && Objects.equals(sort, that.sort)
                && Objects.equals(enabled, that.enabled);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), type, dictCode, dictName, dictValue, desc, sort, enabled);
    }

    public static final class ShenyuDictDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String type;

        private String dictCode;

        private String dictName;

        private String dictValue;

        private String desc;

        private Integer sort;

        private Boolean enabled;

        private ShenyuDictDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return ShenyuDictDOBuilder.
         */
        public ShenyuDictDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return ShenyuDictDOBuilder.
         */
        public ShenyuDictDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return ShenyuDictDOBuilder.
         */
        public ShenyuDictDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * type.
         *
         * @param type the type.
         * @return ShenyuDictDOBuilder.
         */
        public ShenyuDictDOBuilder type(final String type) {
            this.type = type;
            return this;
        }

        /**
         * dictCode.
         *
         * @param dictCode the dictCode.
         * @return ShenyuDictDOBuilder.
         */
        public ShenyuDictDOBuilder dictCode(final String dictCode) {
            this.dictCode = dictCode;
            return this;
        }

        /**
         * dictName.
         *
         * @param dictName the dictName.
         * @return ShenyuDictDOBuilder.
         */
        public ShenyuDictDOBuilder dictName(final String dictName) {
            this.dictName = dictName;
            return this;
        }

        /**
         * dictValue.
         *
         * @param dictValue the dictValue.
         * @return ShenyuDictDOBuilder.
         */
        public ShenyuDictDOBuilder dictValue(final String dictValue) {
            this.dictValue = dictValue;
            return this;
        }

        /**
         * desc.
         *
         * @param desc the desc.
         * @return ShenyuDictDOBuilder.
         */
        public ShenyuDictDOBuilder desc(final String desc) {
            this.desc = desc;
            return this;
        }

        /**
         * sort.
         *
         * @param sort the sort.
         * @return ShenyuDictDOBuilder.
         */
        public ShenyuDictDOBuilder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }

        /**
         * enabled.
         *
         * @param enabled the enabled.
         * @return ShenyuDictDOBuilder.
         */
        public ShenyuDictDOBuilder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public ShenyuDictDO build() {
            ShenyuDictDO shenyuDictDO = new ShenyuDictDO();
            shenyuDictDO.setId(id);
            shenyuDictDO.setDateCreated(dateCreated);
            shenyuDictDO.setDateUpdated(dateUpdated);
            shenyuDictDO.setType(type);
            shenyuDictDO.setDictCode(dictCode);
            shenyuDictDO.setDictName(dictName);
            shenyuDictDO.setDictValue(dictValue);
            shenyuDictDO.setDesc(desc);
            shenyuDictDO.setSort(sort);
            shenyuDictDO.setEnabled(enabled);
            return shenyuDictDO;
        }
    }
}
