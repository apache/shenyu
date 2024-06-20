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
 * this is plugin query.
 */
public class PluginQuery implements Serializable {

    private static final long serialVersionUID = 167659024501717438L;

    /**
     * plugin name.
     */
    private String name;

    /**
     * plugin enabled.
     */
    private Integer enabled;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;

    public PluginQuery() {
    }

    public PluginQuery(final String name, final Integer enabled, final PageParameter pageParameter) {
        this.name = name;
        this.enabled = enabled;
        this.pageParameter = pageParameter;
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
     * Gets the value of enabled.
     *
     * @return the value of enabled
     */
    public Integer getEnabled() {
        return enabled;
    }

    /**
     * Sets the enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final Integer enabled) {
        this.enabled = enabled;
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
        if (!(o instanceof PluginQuery)) {
            return false;
        }
        PluginQuery that = (PluginQuery) o;
        return Objects.equals(name, that.name) && Objects.equals(enabled, that.enabled) && Objects.equals(pageParameter, that.pageParameter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, enabled, pageParameter);
    }
}
