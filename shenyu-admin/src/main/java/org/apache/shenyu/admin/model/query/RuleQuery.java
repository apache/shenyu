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

import java.util.Objects;

/**
 * this is rule query.
 */
public class RuleQuery extends FilterQuery {

    private static final long serialVersionUID = -8048484230783429888L;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * rule name.
     */
    private String name;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;

    public RuleQuery() {
    }

    public RuleQuery(final String selectorId, final String name, final PageParameter pageParameter) {
        this.selectorId = selectorId;
        this.name = name;
        this.pageParameter = pageParameter;
    }

    /**
     * Gets the value of selectorId.
     *
     * @return the value of selectorId
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * Sets the selectorId.
     *
     * @param selectorId selectorId
     */
    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
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
        if (!(o instanceof RuleQuery)) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        RuleQuery ruleQuery = (RuleQuery) o;
        return Objects.equals(selectorId, ruleQuery.selectorId) && Objects.equals(name, ruleQuery.name) && Objects.equals(pageParameter, ruleQuery.pageParameter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), selectorId, name, pageParameter);
    }
}
