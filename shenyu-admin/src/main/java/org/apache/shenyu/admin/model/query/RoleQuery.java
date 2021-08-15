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
import java.util.Objects;

/**
 * The Role Query.
 */
public class RoleQuery implements Serializable {

    private static final long serialVersionUID = -2876510433944603583L;

    /**
     * Role name.
     */
    private String roleName;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;

    public RoleQuery() {
    }

    public RoleQuery(final String roleName, final PageParameter pageParameter) {
        this.roleName = roleName;
        this.pageParameter = pageParameter;
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
     * Gets the value of pageParameter.
     *
     * @return the value of pageParameter
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
        if (!(o instanceof RoleQuery)) {
            return false;
        }
        RoleQuery roleQuery = (RoleQuery) o;
        return Objects.equals(roleName, roleQuery.roleName) && Objects.equals(pageParameter, roleQuery.pageParameter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(roleName, pageParameter);
    }
}
