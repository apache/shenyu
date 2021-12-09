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

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Objects;

/**
 * this shenyu dict from web front.
 */
public class ShenyuDictDTO implements Serializable {

    private static final long serialVersionUID = -8639439741130267709L;

    /**
     * primary key.
     */
    private String id;

    /**
     * dict type.
     */
    @NotNull
    private String type;

    /**
     * dict code.
     */
    private String dictCode;

    /**
     * dict name.
     */
    @NotNull
    private String dictName;

    /**
     * dict value.
     */
    @NotNull
    private String dictValue;

    /**
     * dict desc.
     */
    private String desc;

    /**
     * sort no.
     */
    @NotNull
    private Integer sort;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    public ShenyuDictDTO() {
    }

    public ShenyuDictDTO(final String id,
                         @NotNull final String type,
                         final String dictCode,
                         @NotNull final String dictName,
                         @NotNull final String dictValue,
                         final String desc,
                         @NotNull final Integer sort,
                         final Boolean enabled) {
        this.id = id;
        this.type = type;
        this.dictCode = dictCode;
        this.dictName = dictName;
        this.dictValue = dictValue;
        this.desc = desc;
        this.sort = sort;
        this.enabled = enabled;
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
    public static ShenyuDictDTO.ShenyuDictDTOBuilder builder() {
        return new ShenyuDictDTO.ShenyuDictDTOBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ShenyuDictDTO)) {
            return false;
        }
        ShenyuDictDTO that = (ShenyuDictDTO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(type, that.type)
                && Objects.equals(dictCode, that.dictCode)
                && Objects.equals(dictName, that.dictName)
                && Objects.equals(dictValue, that.dictValue)
                && Objects.equals(desc, that.desc)
                && Objects.equals(sort, that.sort)
                && Objects.equals(enabled, that.enabled);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, type, dictCode, dictName, dictValue, desc, sort, enabled);
    }

    public static final class ShenyuDictDTOBuilder {

        private String id;

        private String type;

        private String dictCode;

        private String dictName;

        private String dictValue;

        private String desc;

        private Integer sort;

        private Boolean enabled;

        private ShenyuDictDTOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return ResourceDTOBuilder.
         */
        public ShenyuDictDTOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * type.
         *
         * @param type the type.
         * @return ShenyuDictDTOBuilder.
         */
        public ShenyuDictDTOBuilder type(final String type) {
            this.type = type;
            return this;
        }

        /**
         * dictCode.
         *
         * @param dictCode the dictCode.
         * @return ShenyuDictDTOBuilder.
         */
        public ShenyuDictDTOBuilder dictCode(final String dictCode) {
            this.dictCode = dictCode;
            return this;
        }

        /**
         * dictName.
         *
         * @param dictName the dictName.
         * @return ShenyuDictDTOBuilder.
         */
        public ShenyuDictDTOBuilder dictName(final String dictName) {
            this.dictName = dictName;
            return this;
        }

        /**
         * dictValue.
         *
         * @param dictValue the dictValue.
         * @return ShenyuDictDTOBuilder.
         */
        public ShenyuDictDTOBuilder dictValue(final String dictValue) {
            this.dictValue = dictValue;
            return this;
        }

        /**
         * desc.
         *
         * @param desc the desc.
         * @return ShenyuDictDTOBuilder.
         */
        public ShenyuDictDTOBuilder desc(final String desc) {
            this.desc = desc;
            return this;
        }

        /**
         * sort.
         *
         * @param sort the sort.
         * @return ShenyuDictDTOBuilder.
         */
        public ShenyuDictDTOBuilder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }

        /**
         * enabled.
         *
         * @param enabled the enabled.
         * @return ShenyuDictDTOBuilder.
         */
        public ShenyuDictDTOBuilder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public ShenyuDictDTO build() {
            return new ShenyuDictDTO(id, type, dictCode, dictName, dictValue, desc, sort, enabled);
        }
    }
}
