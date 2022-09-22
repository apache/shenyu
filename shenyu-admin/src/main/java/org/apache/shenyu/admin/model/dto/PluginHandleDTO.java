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

import org.apache.shenyu.admin.mapper.PluginHandleMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Objects;

/**
 * this plugin handle from web front.
 */
public class PluginHandleDTO implements Serializable {
    
    private static final long serialVersionUID = 8010034956423631265L;
    
    /**
     * primary key.
     */
    @Existed(provider = PluginHandleMapper.class, nullOfIgnore = true, message = "rule not exited")
    private String id;
    
    /**
     * plugin id.
     */
    @NotBlank
    private String pluginId;
    
    /**
     * the field name.
     */
    @NotBlank
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
    @NotNull
    private Integer dataType;
    
    /**
     * the field type.
     * 1  selector,
     * 2  rule.
     */
    @NotNull
    private Integer type;
    
    /**
     * the attribute sort.
     */
    @NotNull
    private Integer sort;
    
    /**
     * the attribute extObj.
     */
    private String extObj;
    
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
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof PluginHandleDTO)) {
            return false;
        }
        PluginHandleDTO that = (PluginHandleDTO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(pluginId, that.pluginId)
                && Objects.equals(field, that.field)
                && Objects.equals(label, that.label)
                && Objects.equals(dataType, that.dataType)
                && Objects.equals(type, that.type)
                && Objects.equals(sort, that.sort)
                && Objects.equals(extObj, that.extObj);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(id, pluginId, field, label, dataType, type, sort, extObj);
    }
}
