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

import javax.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.Objects;

/**
 * data permission dto.
 */
public class DataPermissionDTO implements Serializable {
    private static final long serialVersionUID = -5977862582790251842L;

    /**
     * primary key.
     */
    private String id;

    /**
     * user id.
     */
    @NotBlank
    private String userId;

    /**
     * selector or rule id.
     */
    @NotBlank
    private String dataId;

    /**
     * data type: 0: selector,1 rule.
     */
    private Integer dataType;

    /**
     * whether checkbox is selected.
     */
    private Boolean isSelected;

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
     * Gets the value of userId.
     *
     * @return the value of userId
     */
    public String getUserId() {
        return userId;
    }

    /**
     * Sets the userId.
     *
     * @param userId userId
     */
    public void setUserId(final String userId) {
        this.userId = userId;
    }

    /**
     * Gets the value of dataId.
     *
     * @return the value of dataId
     */
    public String getDataId() {
        return dataId;
    }

    /**
     * Sets the dataId.
     *
     * @param dataId dataId
     */
    public void setDataId(final String dataId) {
        this.dataId = dataId;
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
     * Gets the value of isSelected.
     *
     * @return the value of isSelected
     */
    public Boolean getIsSelected() {
        return isSelected;
    }

    /**
     * Sets the isSelected.
     *
     * @param isSelected isSelected
     */
    public void setIsSelected(final Boolean isSelected) {
        this.isSelected = isSelected;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof DataPermissionDTO)) {
            return false;
        }
        DataPermissionDTO that = (DataPermissionDTO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(userId, that.userId)
                && Objects.equals(dataId, that.dataId)
                && Objects.equals(dataType, that.dataType)
                && Objects.equals(isSelected, that.isSelected);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, userId, dataId, dataType, isSelected);
    }
}
