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
import javax.validation.constraints.NotNull;

/**
 * this is created resource Dto.
 */
public class CreateResourceDTO {

    /**
     * resource parent key.
     */
    @NotBlank
    private String parentId;

    /**
     * resource title.
     */
    @NotBlank
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
    @NotNull
    private Integer resourceType;

    /**
     * resource sort.
     */
    @NotNull
    private Integer sort;

    /**
     * resource icon.
     */
    @NotBlank
    private String icon;

    /**
     * resource is leaf.
     */
    @NotNull
    private Boolean isLeaf;

    /**
     * resource is route.
     */
    @NotNull
    private Integer isRoute;

    /**
     * resource perms.
     */
    private String perms;

    /**
     * resource status.
     */
    @NotNull
    private Integer status;

    public CreateResourceDTO() {
    }

    public CreateResourceDTO(final String parentId, final String title, final String name, final String url, final String component,
                             final Integer resourceType, final Integer sort, final String icon,
                             final Boolean isLeaf, final Integer isRoute, final String perms, final Integer status) {
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
}
