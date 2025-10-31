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

package org.apache.shenyu.common.dto;

/**
 * Base class for common data transfer objects.
 * Contains common fields shared across SelectorData, RuleData, and PluginData.
 */

public class BaseData {

    /**
     * primary key id.
     */
    private String id;

    /**
     * name.
     */
    private String name;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * sort order.
     */
    private Integer sort;

    /**
     * namespace id.
     */
    private String namespaceId;

    /**
     * get id.
     *
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * set id.
     *
     * @param id id
     * @return this
     */
    public BaseData setId(final String id) {
        this.id = id;
        return this;
    }

    /**
     * get name.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * set name.
     *
     * @param name name
     * @return this
     */
    public BaseData setName(final String name) {
        this.name = name;
        return this;
    }

    /**
     * get enabled.
     *
     * @return enabled
     */
    public Boolean getEnabled() {
        return enabled;
    }

    /**
     * set enabled.
     *
     * @param enabled enabled
     * @return this
     */
    public BaseData setEnabled(final Boolean enabled) {
        this.enabled = enabled;
        return this;
    }

    /**
     * get sort.
     *
     * @return sort
     */
    public Integer getSort() {
        return sort;
    }

    /**
     * set sort.
     *
     * @param sort sort
     * @return this
     */
    public BaseData setSort(final Integer sort) {
        this.sort = sort;
        return this;
    }

    /**
     * get namespaceId.
     *
     * @return namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * set namespaceId.
     *
     * @param namespaceId namespaceId
     * @return this
     */
    public BaseData setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
        return this;
    }
}
