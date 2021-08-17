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

import org.apache.shenyu.admin.model.dto.PermissionDTO;
import org.apache.shenyu.common.utils.UUIDUtils;
import reactor.util.StringUtils;

import java.sql.Timestamp;
import java.util.Objects;
import java.util.Optional;

/**
 * The Permission Entity.
 */
public final class PermissionDO extends BaseDO {

    private static final long serialVersionUID = 8371869040638596986L;

    /**
     * user key or role key.
     */
    private String objectId;

    /**
     * resource key.
     */
    private String resourceId;

    public PermissionDO() {
    }

    public PermissionDO(final String objectId, final String resourceId) {
        this.objectId = objectId;
        this.resourceId = resourceId;
    }

    /**
     * Gets the value of objectId.
     *
     * @return the value of objectId
     */
    public String getObjectId() {
        return objectId;
    }

    /**
     * Sets the objectId.
     *
     * @param objectId objectId
     */
    public void setObjectId(final String objectId) {
        this.objectId = objectId;
    }

    /**
     * Gets the value of resourceId.
     *
     * @return the value of resourceId
     */
    public String getResourceId() {
        return resourceId;
    }

    /**
     * Sets the resourceId.
     *
     * @param resourceId resourceId
     */
    public void setResourceId(final String resourceId) {
        this.resourceId = resourceId;
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static PermissionDO.PermissionDOBuilder builder() {
        return new PermissionDO.PermissionDOBuilder();
    }

    /**
     * build Permission DO.
     *
     * @param permissionDTO {@linkplain PermissionDTO}
     * @return {@linkplain PermissionDO}
     */
    public static PermissionDO buildPermissionDO(final PermissionDTO permissionDTO) {
        return Optional.ofNullable(permissionDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            PermissionDO permissionDO = PermissionDO.builder()
                    .objectId(item.getObjectId())
                    .resourceId(item.getResourceId())
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                permissionDO.setId(UUIDUtils.getInstance().generateShortUuid());
                permissionDO.setDateCreated(currentTime);
            } else {
                permissionDO.setId(item.getId());
            }
            return permissionDO;
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
        PermissionDO that = (PermissionDO) o;
        return Objects.equals(objectId, that.objectId) && Objects.equals(resourceId, that.resourceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), objectId, resourceId);
    }

    public static final class PermissionDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String objectId;

        private String resourceId;

        private PermissionDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return PermissionDOBuilder.
         */
        public PermissionDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return PermissionDOBuilder.
         */
        public PermissionDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return PermissionDOBuilder.
         */
        public PermissionDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * objectId.
         *
         * @param objectId the objectId.
         * @return PermissionDOBuilder.
         */
        public PermissionDOBuilder objectId(final String objectId) {
            this.objectId = objectId;
            return this;
        }

        /**
         * resourceId.
         *
         * @param resourceId the resourceId.
         * @return PermissionDOBuilder.
         */
        public PermissionDOBuilder resourceId(final String resourceId) {
            this.resourceId = resourceId;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public PermissionDO build() {
            PermissionDO permissionDO = new PermissionDO();
            permissionDO.setId(id);
            permissionDO.setDateCreated(dateCreated);
            permissionDO.setDateUpdated(dateUpdated);
            permissionDO.setObjectId(objectId);
            permissionDO.setResourceId(resourceId);
            return permissionDO;
        }
    }
}
