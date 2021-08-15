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

import org.apache.shenyu.admin.model.entity.RoleDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.Objects;
import java.util.Optional;

/**
 * this is role view to web front.
 */
public class RoleVO implements Serializable {

    private static final long serialVersionUID = 2783609252111382305L;

    /**
     * primary key.
     */
    private String id;

    /**
     * role name.
     */
    private String roleName;

    /**
     * description.
     */
    private String description;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    public RoleVO() {
    }

    public RoleVO(final String id, final String roleName, final String description, final String dateCreated, final String dateUpdated) {
        this.id = id;
        this.roleName = roleName;
        this.description = description;
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
     * Gets the value of roleName.
     *
     * @return the value of roleName
     */
    public String getRoleName() {
        return roleName;
    }

    /**
     * Sets the roleName.
     *
     * @param roleName roleName
     */
    public void setRoleName(final String roleName) {
        this.roleName = roleName;
    }

    /**
     * Gets the value of description.
     *
     * @return the value of description
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the description.
     *
     * @param description description
     */
    public void setDescription(final String description) {
        this.description = description;
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
     * build roleVO.
     *
     * @param roleDO {@linkplain RoleDO}
     * @return {@linkplain RoleVO}
     */
    public static RoleVO buildRoleVO(final RoleDO roleDO) {
        return Optional.ofNullable(roleDO)
                .map(item -> new RoleVO(item.getId(), item.getRoleName(),
                        item.getDescription(), DateUtils.localDateTimeToString(item.getDateCreated().toLocalDateTime()),
                        DateUtils.localDateTimeToString(item.getDateUpdated().toLocalDateTime()))).orElse(null);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof RoleVO)) {
            return false;
        }
        RoleVO roleVO = (RoleVO) o;
        return Objects.equals(id, roleVO.id)
                && Objects.equals(roleName, roleVO.roleName)
                && Objects.equals(description, roleVO.description)
                && Objects.equals(dateCreated, roleVO.dateCreated)
                && Objects.equals(dateUpdated, roleVO.dateUpdated);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, roleName, description, dateCreated, dateUpdated);
    }

    @Override
    public String toString() {
        return "RoleVO{"
                + "id='" + id + '\''
                + ", roleName='" + roleName + '\''
                + ", description='" + description + '\''
                + ", dateCreated='" + dateCreated + '\''
                + ", dateUpdated='" + dateUpdated + '\''
                + '}';
    }
}
