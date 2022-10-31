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

import org.apache.shenyu.admin.mapper.RoleMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * this is role from by web front.
 */
public class RoleDTO implements Serializable {
    
    private static final long serialVersionUID = -3017693566893175737L;
    
    /**
     * primary key.
     */
    @Existed(provider = RoleMapper.class, nullOfIgnore = true, message = "role is not existed")
    private String id;
    
    /**
     * role name.
     */
    @NotBlank
    private String roleName;
    
    /**
     * description.
     */
    private String description;
    
    /**
     * pre permission ids.
     */
    private List<@NotBlank String> currentPermissionIds;
    
    public RoleDTO() {
    }
    
    public RoleDTO(final String id, @NotBlank final String roleName, final String description, final List<String> currentPermissionIds) {
        this.id = id;
        this.roleName = roleName;
        this.description = description;
        this.currentPermissionIds = currentPermissionIds;
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
     * Gets the value of currentPermissionIds.
     *
     * @return the value of currentPermissionIds
     */
    public List<String> getCurrentPermissionIds() {
        return currentPermissionIds;
    }
    
    /**
     * Sets the currentPermissionIds.
     *
     * @param currentPermissionIds currentPermissionIds
     */
    public void setCurrentPermissionIds(final List<String> currentPermissionIds) {
        this.currentPermissionIds = currentPermissionIds;
    }
    
    /**
     * builder method.
     *
     * @return builder object.
     */
    public static RoleDTO.RoleDTOBuilder builder() {
        return new RoleDTO.RoleDTOBuilder();
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof RoleDTO)) {
            return false;
        }
        RoleDTO roleDTO = (RoleDTO) o;
        return Objects.equals(id, roleDTO.id)
                && Objects.equals(roleName, roleDTO.roleName)
                && Objects.equals(description, roleDTO.description)
                && Objects.equals(currentPermissionIds, roleDTO.currentPermissionIds);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(id, roleName, description, currentPermissionIds);
    }
    
    public static final class RoleDTOBuilder {
        
        private String id;
        
        private String roleName;
        
        private String description;
        
        private List<String> currentPermissionIds;
        
        private RoleDTOBuilder() {
        }
        
        /**
         * id.
         *
         * @param id the id.
         * @return RoleDTOBuilder.
         */
        public RoleDTOBuilder id(final String id) {
            this.id = id;
            return this;
        }
        
        /**
         * roleName.
         *
         * @param roleName the roleName.
         * @return RoleDTOBuilder.
         */
        public RoleDTOBuilder roleName(final String roleName) {
            this.roleName = roleName;
            return this;
        }
        
        /**
         * description.
         *
         * @param description the description.
         * @return RoleDTOBuilder.
         */
        public RoleDTOBuilder description(final String description) {
            this.description = description;
            return this;
        }
        
        /**
         * currentPermissionIds.
         *
         * @param currentPermissionIds the currentPermissionIds.
         * @return RoleDTOBuilder.
         */
        public RoleDTOBuilder currentPermissionIds(final List<String> currentPermissionIds) {
            this.currentPermissionIds = currentPermissionIds;
            return this;
        }
        
        /**
         * build method.
         *
         * @return build object.
         */
        public RoleDTO build() {
            RoleDTO roleDTO = new RoleDTO();
            roleDTO.setId(id);
            roleDTO.setRoleName(roleName);
            roleDTO.setDescription(description);
            roleDTO.setCurrentPermissionIds(currentPermissionIds);
            return roleDTO;
        }
    }
}
