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

import java.util.List;
import org.apache.shenyu.admin.model.page.PageParameter;

import java.util.Objects;

/**
 * this is selector query.
 */
public class SelectorQuery extends FilterQuery {

    private static final long serialVersionUID = -1019736306667647529L;

    /**
     * plugin id.
     */
    private String pluginId;

    /**
     * plugin ids.
     */
    private List<String> pluginIds;

    /**
     * selector name.
     */
    private String name;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;

    public SelectorQuery() {
    }

    public SelectorQuery(final String pluginId, final String name, final PageParameter pageParameter) {
        this.pluginId = pluginId;
        this.name = name;
        this.pageParameter = pageParameter;
    }

    public SelectorQuery(final List<String> pluginIds, final String name, final PageParameter pageParameter) {
        this.pluginIds = pluginIds;
        this.name = name;
        this.pageParameter = pageParameter;
    }

    /**
     * Gets the value of pluginId.
     *
     * @return the value of pluginId
     */
    public String getPluginId() {
        return pluginId;
    }

    /**
     * Sets the pluginId.
     *
     * @param pluginId pluginId
     */
    public void setPluginId(final String pluginId) {
        this.pluginId = pluginId;
    }

    /**
     * Gets the value of pluginIds.
     *
     * @return the value of pluginIds
     */
    public List<String> getPluginIds() {
        return pluginIds;
    }

    /**
     * Sets the pluginIds.
     *
     * @param pluginIds pluginIds
     */
    public void setPluginIds(final List<String> pluginIds) {
        this.pluginIds = pluginIds;
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
        if (!(o instanceof SelectorQuery)) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        SelectorQuery that = (SelectorQuery) o;
        return Objects.equals(pluginId, that.pluginId) && Objects.equals(name, that.name) && Objects.equals(pageParameter, that.pageParameter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), pluginId, name, pageParameter);
    }
}
