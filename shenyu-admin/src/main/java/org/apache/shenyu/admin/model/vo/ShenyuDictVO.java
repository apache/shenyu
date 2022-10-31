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

package org.apache.shenyu.admin.model.vo;

import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.Optional;

/**
 * this is shenyu dict view to web front.
 */
public class ShenyuDictVO implements Serializable {

    private static final long serialVersionUID = 5731120468713362319L;

    /**
     * primary key.
     */
    private String id;

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

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    public ShenyuDictVO() {
    }

    public ShenyuDictVO(final String id,
                        final String type,
                        final String dictCode,
                        final String dictName,
                        final String dictValue,
                        final String desc,
                        final Integer sort,
                        final Boolean enabled,
                        final String dateCreated,
                        final String dateUpdated) {
        this.id = id;
        this.type = type;
        this.dictCode = dictCode;
        this.dictName = dictName;
        this.dictValue = dictValue;
        this.desc = desc;
        this.sort = sort;
        this.enabled = enabled;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
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
     * Gets the value of dateCreated.
     *
     * @return the value of dateCreated
     */
    public String getDateCreated() {
        return dateCreated;
    }

    /**
     * Sets the dateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final String dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * Gets the value of dateUpdated.
     *
     * @return the value of dateUpdated
     */
    public String getDateUpdated() {
        return dateUpdated;
    }

    /**
     * Sets the dateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final String dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    /**
     * build {@linkplain ShenyuDictVO}.
     *
     * @param shenyuDictDO {@linkplain ShenyuDictDO}
     * @return {@linkplain ShenyuDictVO}
     */
    public static ShenyuDictVO buildShenyuDictVO(final ShenyuDictDO shenyuDictDO) {
        return Optional.ofNullable(shenyuDictDO)
                .map(it -> new ShenyuDictVO(shenyuDictDO.getId(), shenyuDictDO.getType(),
                        shenyuDictDO.getDictCode(), shenyuDictDO.getDictName(),
                        shenyuDictDO.getDictValue(), shenyuDictDO.getDesc(), shenyuDictDO.getSort(), shenyuDictDO.getEnabled(),
                        DateUtils.localDateTimeToString(shenyuDictDO.getDateCreated().toLocalDateTime()),
                        DateUtils.localDateTimeToString(shenyuDictDO.getDateUpdated().toLocalDateTime())))
                .orElse(null);
    }
}
