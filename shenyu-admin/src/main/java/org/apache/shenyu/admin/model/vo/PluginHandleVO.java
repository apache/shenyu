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

import org.apache.shenyu.admin.model.entity.PluginHandleDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

/**
 * this is plugin handle view to web front.
 */
public class PluginHandleVO implements Serializable {

    private static final long serialVersionUID = 940877592520676748L;

    /**
     * primary key.
     */
    private String id;

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

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    private List<ShenyuDictVO> dictOptions;

    public PluginHandleVO() {
    }

    public PluginHandleVO(final String id,
                          final String pluginId,
                          final String field,
                          final String label,
                          final Integer dataType,
                          final Integer type,
                          final Integer sort,
                          final String extObj,
                          final String dateCreated,
                          final String dateUpdated,
                          final List<ShenyuDictVO> dictOptions) {
        this.id = id;
        this.pluginId = pluginId;
        this.field = field;
        this.label = label;
        this.dataType = dataType;
        this.type = type;
        this.sort = sort;
        this.extObj = extObj;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
        this.dictOptions = dictOptions;
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
     * Gets the value of dictOptions.
     *
     * @return the value of dictOptions
     */
    public List<ShenyuDictVO> getDictOptions() {
        return dictOptions;
    }

    /**
     * Sets the dictOptions.
     *
     * @param dictOptions dictOptions
     */
    public void setDictOptions(final List<ShenyuDictVO> dictOptions) {
        this.dictOptions = dictOptions;
    }

    /**
     * build {@linkplain PluginHandleVO}.
     *
     * @param pluginHandleDO {@linkplain PluginHandleDO}
     * @param dictOptions dictOptions
     * @return {@linkplain PluginHandleVO}
     */
    public static PluginHandleVO buildPluginHandleVO(final PluginHandleDO pluginHandleDO, final List<ShenyuDictVO> dictOptions) {
        return Optional.ofNullable(pluginHandleDO)
                .map(it -> new PluginHandleVO(pluginHandleDO.getId(), pluginHandleDO.getPluginId(),
                        pluginHandleDO.getField(), pluginHandleDO.getLabel(),
                        pluginHandleDO.getDataType(), pluginHandleDO.getType(), pluginHandleDO.getSort(), pluginHandleDO.getExtObj(),
                        DateUtils.localDateTimeToString(pluginHandleDO.getDateCreated().toLocalDateTime()),
                        DateUtils.localDateTimeToString(pluginHandleDO.getDateUpdated().toLocalDateTime()), dictOptions))
                .orElse(null);
    }
}
