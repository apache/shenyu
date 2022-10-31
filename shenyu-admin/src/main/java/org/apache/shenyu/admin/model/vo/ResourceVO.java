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

import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.Objects;
import java.util.Optional;

/**
 * this is resource for web front.
 */
public class ResourceVO implements Serializable {

    private static final long serialVersionUID = -3205569090998147615L;

    /**
     * primary key.
     */
    private String id;

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
    private String name;

    /**
     * resource url.
     */
    private String url;

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
    private Integer sort;

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

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    public ResourceVO() {
    }

    public ResourceVO(final String id,
                      final String parentId,
                      final String title,
                      final String name,
                      final String url,
                      final String component,
                      final Integer resourceType,
                      final Integer sort,
                      final String icon,
                      final Boolean isLeaf,
                      final Integer isRoute,
                      final String perms,
                      final Integer status,
                      final String dateCreated,
                      final String dateUpdated) {
        this.id = id;
        this.parentId = parentId;
        this.title = title;
        this.name = name;
        this.url = url;
        this.component = component;
        this.resourceType = resourceType;
        this.sort = sort;
        this.icon = icon;
        this.isLeaf = isLeaf;
        this.isRoute = isRoute;
        this.perms = perms;
        this.status = status;
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
     * Gets the value of name.
     *
     * @return the value of name
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * Gets the value of url.
     *
     * @return the value of url
     */
    public String getUrl() {
        return url;
    }

    /**
     * Sets the url.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
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
     * build ResourceVO.
     *
     * @param resourceDO {@linkplain ResourceDO}
     * @return {@linkplain ResourceVO}
     */
    public static ResourceVO buildResourceVO(final ResourceDO resourceDO) {
        return Optional.ofNullable(resourceDO).map(item -> new ResourceVO(item.getId(), item.getParentId(),
                item.getTitle(), item.getName(), item.getUrl(), item.getComponent(), item.getResourceType(),
                item.getSort(), item.getIcon(), item.getIsLeaf(), item.getIsRoute(), item.getPerms(), item.getStatus(),
                DateUtils.localDateTimeToString(item.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(item.getDateUpdated().toLocalDateTime()))).orElse(null);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ResourceVO)) {
            return false;
        }
        ResourceVO that = (ResourceVO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(parentId, that.parentId)
                && Objects.equals(title, that.title)
                && Objects.equals(name, that.name)
                && Objects.equals(url, that.url)
                && Objects.equals(component, that.component)
                && Objects.equals(resourceType, that.resourceType)
                && Objects.equals(sort, that.sort)
                && Objects.equals(icon, that.icon)
                && Objects.equals(isLeaf, that.isLeaf)
                && Objects.equals(isRoute, that.isRoute)
                && Objects.equals(perms, that.perms)
                && Objects.equals(status, that.status)
                && Objects.equals(dateCreated, that.dateCreated)
                && Objects.equals(dateUpdated, that.dateUpdated);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, parentId, title, name, url, component, resourceType, sort, icon, isLeaf, isRoute, perms, status, dateCreated, dateUpdated);
    }

    @Override
    public String toString() {
        return "ResourceVO{"
                + "id='" + id + '\''
                + ", parentId='" + parentId + '\''
                + ", title='" + title + '\''
                + ", name='" + name + '\''
                + ", url='" + url + '\''
                + ", component='" + component + '\''
                + ", resourceType=" + resourceType
                + ", sort=" + sort
                + ", icon='" + icon + '\''
                + ", isLeaf=" + isLeaf
                + ", isRoute=" + isRoute
                + ", perms='" + perms + '\''
                + ", status=" + status
                + ", dateCreated='" + dateCreated + '\''
                + ", dateUpdated='" + dateUpdated + '\''
                + '}';
    }
}
