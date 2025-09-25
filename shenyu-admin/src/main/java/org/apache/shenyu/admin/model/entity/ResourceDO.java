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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.CreateResourceDTO;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Optional;

/**
 * The Resource Entity.
 */
public final class ResourceDO extends BaseDO {

    private static final long serialVersionUID = 4663697054300237200L;

    /**
     * resource parent key.
     */
    private String parentId;

    /**
     * resource title.
     */
    private String title;

    /**
     * resource name.
     */
    private String resourceName;

    /**
     * resource url.
     */
    private String resourceUrl;

    /**
     * resource component.
     */
    private String component;

    /**
     * resource type.
     */
    private Integer resourceType;

    /**
     * resource sort.
     */
    private Integer resourceSort;

    /**
     * resource icon.
     */
    private String icon;

    /**
     * resource is leaf.
     */
    private Boolean isLeaf;

    /**
     * resource is route.
     */
    private Integer isRoute;

    /**
     * resource perms.
     */
    private String perms;

    /**
     * resource status.
     */
    private Integer status;

    public ResourceDO() {
    }

    public ResourceDO(final String parentId,
                      final String title,
                      final String resourceName,
                      final String resourceUrl,
                      final String component,
                      final Integer resourceType,
                      final Integer resourceSort,
                      final String icon,
                      final Boolean isLeaf,
                      final Integer isRoute,
                      final String perms,
                      final Integer status) {
        this.parentId = parentId;
        this.title = title;
        this.resourceName = resourceName;
        this.resourceUrl = resourceUrl;
        this.component = component;
        this.resourceType = resourceType;
        this.resourceSort = resourceSort;
        this.icon = icon;
        this.isLeaf = isLeaf;
        this.isRoute = isRoute;
        this.perms = perms;
        this.status = status;
    }

    /**
     * Gets the value of parentId.
     *
     * @return the value of parentId
     */
    public String getParentId() {
        return parentId;
    }

    /**
     * Sets the parentId.
     *
     * @param parentId parentId
     */
    public void setParentId(final String parentId) {
        this.parentId = parentId;
    }

    /**
     * Gets the value of title.
     *
     * @return the value of title
     */
    public String getTitle() {
        return title;
    }

    /**
     * Sets the title.
     *
     * @param title title
     */
    public void setTitle(final String title) {
        this.title = title;
    }

    /**
     * Gets the value of resourceName.
     *
     * @return the value of resourceName
     */
    public String getName() {
        return resourceName;
    }

    /**
     * Sets the resourceName.
     *
     * @param resourceName resourceName
     */
    public void setName(final String resourceName) {
        this.resourceName = resourceName;
    }

    /**
     * Gets the value of resourceUrl.
     *
     * @return the value of resourceUrl
     */
    public String getUrl() {
        return resourceUrl;
    }

    /**
     * Sets the resourceUrl.
     *
     * @param resourceUrl resourceUrl
     */
    public void setUrl(final String resourceUrl) {
        this.resourceUrl = resourceUrl;
    }

    /**
     * Gets the value of component.
     *
     * @return the value of component
     */
    public String getComponent() {
        return component;
    }

    /**
     * Sets the component.
     *
     * @param component component
     */
    public void setComponent(final String component) {
        this.component = component;
    }

    /**
     * Gets the value of resourceType.
     *
     * @return the value of resourceType
     */
    public Integer getResourceType() {
        return resourceType;
    }

    /**
     * Sets the resourceType.
     *
     * @param resourceType resourceType
     */
    public void setResourceType(final Integer resourceType) {
        this.resourceType = resourceType;
    }

    /**
     * Gets the value of resourceSort.
     *
     * @return the value of resourceSort
     */
    public Integer getSort() {
        return resourceSort;
    }

    /**
     * Sets the resourceSort.
     *
     * @param resourceSort resourceSort
     */
    public void setSort(final Integer resourceSort) {
        this.resourceSort = resourceSort;
    }

    /**
     * Gets the value of icon.
     *
     * @return the value of icon
     */
    public String getIcon() {
        return icon;
    }

    /**
     * Sets the icon.
     *
     * @param icon icon
     */
    public void setIcon(final String icon) {
        this.icon = icon;
    }

    /**
     * Gets the value of isLeaf.
     *
     * @return the value of isLeaf
     */
    public Boolean getIsLeaf() {
        return isLeaf;
    }

    /**
     * Sets the isLeaf.
     *
     * @param isLeaf isLeaf
     */
    public void setIsLeaf(final Boolean isLeaf) {
        this.isLeaf = isLeaf;
    }

    /**
     * Gets the value of isRoute.
     *
     * @return the value of isRoute
     */
    public Integer getIsRoute() {
        return isRoute;
    }

    /**
     * Sets the isRoute.
     *
     * @param isRoute isRoute
     */
    public void setIsRoute(final Integer isRoute) {
        this.isRoute = isRoute;
    }

    /**
     * Gets the value of perms.
     *
     * @return the value of perms
     */
    public String getPerms() {
        return perms;
    }

    /**
     * Sets the perms.
     *
     * @param perms perms
     */
    public void setPerms(final String perms) {
        this.perms = perms;
    }

    /**
     * Gets the value of status.
     *
     * @return the value of status
     */
    public Integer getStatus() {
        return status;
    }

    /**
     * Sets the status.
     *
     * @param status status
     */
    public void setStatus(final Integer status) {
        this.status = status;
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static ResourceDO.ResourceDOBuilder builder() {
        return new ResourceDO.ResourceDOBuilder();
    }

    /**
     * build resourceDO.
     *
     * @param resourceDTO {@linkplain ResourceDTO}
     * @return {@linkplain ResourceDO}
     */
    public static ResourceDO buildResourceDO(final ResourceDTO resourceDTO) {
        return Optional.ofNullable(resourceDTO).map(item -> {
            Timestamp currentTime = Timestamp.valueOf(LocalDateTime.now());
            ResourceDO resourceDO = ResourceDO.builder()
                    .id(item.getId())
                    .parentId(item.getParentId())
                    .title(item.getTitle())
                    .resourceName(item.getName())
                    .resourceUrl(item.getUrl())
                    .component(item.getComponent())
                    .resourceType(item.getResourceType())
                    .resourceSort(item.getSort())
                    .icon(item.getIcon())
                    .isLeaf(item.getIsLeaf())
                    .isRoute(item.getIsRoute())
                    .perms(item.getPerms())
                    .status(item.getStatus())
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                resourceDO.setId(UUIDUtils.getInstance().generateShortUuid());
                resourceDO.setDateCreated(currentTime);
            }
            resourceDO.setDateUpdated(currentTime);
            return resourceDO;
        }).orElse(null);
    }

    /**
     * build resourceDO.
     *
     * @param createResourceDTO {@linkplain CreateResourceDTO}
     * @return {@linkplain ResourceDO}
     */
    public static ResourceDO buildResourceDO(final CreateResourceDTO createResourceDTO) {
        return Optional.ofNullable(createResourceDTO).map(item -> {
            Timestamp currentTime = Timestamp.valueOf(LocalDateTime.now());
            return ResourceDO.builder()
                    .id(UUIDUtils.getInstance().generateShortUuid())
                    .parentId(item.getParentId())
                    .title(item.getTitle())
                    .resourceName(item.getName())
                    .resourceUrl(item.getUrl())
                    .component(item.getComponent())
                    .resourceType(item.getResourceType())
                    .resourceSort(item.getSort())
                    .icon(item.getIcon())
                    .isLeaf(item.getIsLeaf())
                    .isRoute(item.getIsRoute())
                    .perms(item.getPerms())
                    .status(item.getStatus())
                    .dateCreated(currentTime)
                    .dateUpdated(currentTime)
                    .build();
        }).orElse(null);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        ResourceDO that = (ResourceDO) o;
        return Objects.equals(parentId, that.parentId)
                && Objects.equals(title, that.title)
                && Objects.equals(resourceName, that.resourceName)
                && Objects.equals(resourceUrl, that.resourceUrl)
                && Objects.equals(component, that.component)
                && Objects.equals(resourceType, that.resourceType)
                && Objects.equals(resourceSort, that.resourceSort)
                && Objects.equals(icon, that.icon)
                && Objects.equals(isLeaf, that.isLeaf)
                && Objects.equals(isRoute, that.isRoute)
                && Objects.equals(perms, that.perms)
                && Objects.equals(status, that.status);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), parentId, title, resourceName, resourceUrl, component, resourceType, resourceSort, icon, isLeaf, isRoute, perms, status);
    }

    public static final class ResourceDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String parentId;

        private String title;

        private String resourceName;

        private String resourceUrl;

        private String component;

        private Integer resourceType;

        private Integer resourceSort;

        private String icon;

        private Boolean isLeaf;

        private Integer isRoute;

        private String perms;

        private Integer status;

        private ResourceDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * parentId.
         *
         * @param parentId the parentId.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder parentId(final String parentId) {
            this.parentId = parentId;
            return this;
        }

        /**
         * title.
         *
         * @param title the title.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder title(final String title) {
            this.title = title;
            return this;
        }

        /**
         * resourceName.
         *
         * @param resourceName the resourceName.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder resourceName(final String resourceName) {
            this.resourceName = resourceName;
            return this;
        }

        /**
         * resourceUrl.
         *
         * @param resourceUrl the resourceUrl.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder resourceUrl(final String resourceUrl) {
            this.resourceUrl = resourceUrl;
            return this;
        }

        /**
         * component.
         *
         * @param component the component.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder component(final String component) {
            this.component = component;
            return this;
        }

        /**
         * resourceType.
         *
         * @param resourceType the resourceType.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder resourceType(final Integer resourceType) {
            this.resourceType = resourceType;
            return this;
        }

        /**
         * resourceSort.
         *
         * @param resourceSort the resourceSort.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder resourceSort(final Integer resourceSort) {
            this.resourceSort = resourceSort;
            return this;
        }

        /**
         * icon.
         *
         * @param icon the icon.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder icon(final String icon) {
            this.icon = icon;
            return this;
        }

        /**
         * isLeaf.
         *
         * @param isLeaf the isLeaf.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder isLeaf(final Boolean isLeaf) {
            this.isLeaf = isLeaf;
            return this;
        }

        /**
         * isRoute.
         *
         * @param isRoute the isRoute.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder isRoute(final Integer isRoute) {
            this.isRoute = isRoute;
            return this;
        }

        /**
         * perms.
         *
         * @param perms the perms.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder perms(final String perms) {
            this.perms = perms;
            return this;
        }

        /**
         * status.
         *
         * @param status the status.
         * @return ResourceDOBuilder.
         */
        public ResourceDOBuilder status(final Integer status) {
            this.status = status;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public ResourceDO build() {
            ResourceDO resourceDO = new ResourceDO(parentId, title, resourceName, resourceUrl, component, resourceType, resourceSort, icon, isLeaf, isRoute, perms, status);
            resourceDO.setId(id);
            resourceDO.setDateCreated(dateCreated);
            resourceDO.setDateUpdated(dateUpdated);
            return resourceDO;
        }
    }
}
