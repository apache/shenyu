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

import org.apache.shenyu.admin.model.dto.RoleDTO;
import org.apache.shenyu.common.utils.UUIDUtils;
import reactor.util.StringUtils;

import java.sql.Timestamp;
import java.util.Objects;
import java.util.Optional;

/**
 * The Role Data Entity.
 */
public final class RoleDO extends BaseDO {

    private static final long serialVersionUID = -7319631396664845158L;

    /**
     * role name.
     */
    private String roleName;

    /**
     * description.
     */
    private String description;

    public RoleDO() {
    }

    public RoleDO(final String roleName, final String description) {
        this.roleName = roleName;
        this.description = description;
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
     * builder method.
     *
     * @return builder object.
     */
    public static RoleDO.RoleDOBuilder builder() {
        return new RoleDO.RoleDOBuilder();
    }

    /**
     * build RoleDO.
     *
     * @param roleDTO {@linkplain RoleDTO}
     * @return {@linkplain RoleDO}
     */
    public static RoleDO buildRoleDO(final RoleDTO roleDTO) {
        return Optional.ofNullable(roleDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            RoleDO roleDO = RoleDO.builder()
                    .roleName(item.getRoleName())
                    .description(item.getDescription())
                    .dateUpdated(currentTime)
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                roleDO.setId(UUIDUtils.getInstance().generateShortUuid());
                roleDO.setDateCreated(currentTime);
            } else {
                roleDO.setId(item.getId());
            }
            return roleDO;
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
        RoleDO roleDO = (RoleDO) o;
        return Objects.equals(roleName, roleDO.roleName) && Objects.equals(description, roleDO.description);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), roleName, description);
    }

    public static final class RoleDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String roleName;

        private String description;

        private RoleDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return RoleDOBuilder.
         */
        public RoleDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return RoleDOBuilder.
         */
        public RoleDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return RoleDOBuilder.
         */
        public RoleDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * roleName.
         *
         * @param roleName the roleName.
         * @return RoleDOBuilder.
         */
        public RoleDOBuilder roleName(final String roleName) {
            this.roleName = roleName;
            return this;
        }

        /**
         * description.
         *
         * @param description the description.
         * @return RoleDOBuilder.
         */
        public RoleDOBuilder description(final String description) {
            this.description = description;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public RoleDO build() {
            RoleDO roleDO = new RoleDO();
            roleDO.setId(id);
            roleDO.setDateCreated(dateCreated);
            roleDO.setDateUpdated(dateUpdated);
            roleDO.setRoleName(roleName);
            roleDO.setDescription(description);
            return roleDO;
        }
    }
}
