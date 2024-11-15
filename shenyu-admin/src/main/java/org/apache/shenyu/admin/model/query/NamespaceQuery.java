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

package org.apache.shenyu.admin.model.query;

import org.apache.shenyu.admin.model.page.PageParameter;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * this is application authority query.
 */
public class NamespaceQuery implements Serializable {


    /**
     * namespace id.
     */
    private String namespaceId;

    /**
     * namespace ids.
     */
    private List<String> namespaceIds;

    /**
     * name.
     */
    private String name;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;

    /**
     * Gets the value of namespaceId.
     *
     * @return the value of namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * Sets the namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }
    
    /**
     * Gets the value of namespaceIds.
     *
     * @return the value of namespaceIds
     */
    public List<String> getNamespaceIds() {
        return namespaceIds;
    }
    
    /**
     * Sets the namespaceIds.
     *
     * @param namespaceIds namespaceIds
     */
    public void setNamespaceIds(final List<String> namespaceIds) {
        this.namespaceIds = namespaceIds;
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
     * Gets the value of getPageParameter.
     *
     * @return the value of getPageParameter
     */
    public PageParameter getPageParameter() {
        return pageParameter;
    }

    /**
     * Sets the pageParameter.
     *
     * @param pageParameter pageParameter
     */
    public void setPageParameter(final PageParameter pageParameter) {
        this.pageParameter = pageParameter;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        NamespaceQuery that = (NamespaceQuery) o;
        return Objects.equals(namespaceId, that.namespaceId) && Objects.equals(name, that.name)
                && Objects.equals(pageParameter, that.pageParameter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(namespaceId, name, pageParameter);
    }
}
