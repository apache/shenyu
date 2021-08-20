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

import java.util.Objects;

/**
 * this is permission Dto.
 */
public class PermissionDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * user key or role key.
     */
    private String objectId;

    /**
     * resource key.
     */
    private String resourceId;

    public PermissionDTO() {
    }

    public PermissionDTO(final String id, final String objectId, final String resourceId) {
        this.id = id;
        this.objectId = objectId;
        this.resourceId = resourceId;
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
    public static PermissionDTO.PermissionDTOBuilder builder() {
        return new PermissionDTO.PermissionDTOBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof PermissionDTO)) {
            return false;
        }
        PermissionDTO that = (PermissionDTO) o;
        return Objects.equals(id, that.id) && Objects.equals(objectId, that.objectId) && Objects.equals(resourceId, that.resourceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, objectId, resourceId);
    }

    public static final class PermissionDTOBuilder {

        private String id;

        private String objectId;

        private String resourceId;

        private PermissionDTOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return PermissionDTOBuilder.
         */
        public PermissionDTOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * objectId.
         *
         * @param objectId the objectId.
         * @return PermissionDTOBuilder.
         */
        public PermissionDTOBuilder objectId(final String objectId) {
            this.objectId = objectId;
            return this;
        }

        /**
         * resourceId.
         *
         * @param resourceId the resourceId.
         * @return PermissionDTOBuilder.
         */
        public PermissionDTOBuilder resourceId(final String resourceId) {
            this.resourceId = resourceId;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public PermissionDTO build() {
            PermissionDTO permissionDTO = new PermissionDTO();
            permissionDTO.setId(id);
            permissionDTO.setObjectId(objectId);
            permissionDTO.setResourceId(resourceId);
            return permissionDTO;
        }
    }
}
