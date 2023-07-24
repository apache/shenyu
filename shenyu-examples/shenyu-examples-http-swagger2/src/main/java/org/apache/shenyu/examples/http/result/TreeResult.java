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

package org.apache.shenyu.examples.http.result;

import io.swagger.annotations.ApiModelProperty;
import java.util.List;

public class TreeResult {
    @ApiModelProperty(value = "id", required = true, example = "123")
    private Integer id;

    @ApiModelProperty(value = "name", required = true, example = "shenyu")
    private String name;

    @ApiModelProperty(value = "parent id")
    private Integer parentId;

    @ApiModelProperty(value = "children node list", example = "list")
    private List<TreeResult> children;

    /**
     * get id.
     *
     * @return Integer
     */
    public Integer getId() {
        return id;
    }

    /**
     * setId.
     *
     * @param id id
     */
    public void setId(final Integer id) {
        this.id = id;
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
     * setName.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * getParentId.
     *
     * @return parentId
     */
    public Integer getParentId() {
        return parentId;
    }

    /**
     * setParentId.
     *
     * @param parentId parentId
     */
    public void setParentId(final Integer parentId) {
        this.parentId = parentId;
    }

    /**
     * getChildren.
     *
     * @return list
     */
    public List<TreeResult> getChildren() {
        return children;
    }

    /**
     * setChildren.
     *
     * @param children children
     */
    public void setChildren(final List<TreeResult> children) {
        this.children = children;
    }
}
