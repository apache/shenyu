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
 * this is plugin handle query.
 */
public class PluginHandleQuery {
    /**
     * plugin id.
     */
    private String pluginId;

    /**
     * plugin field.
     */
    private String field;

    /**
     *  type.
     *  1  selector,
     *  2  rule
     */
    private Integer type;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;

    public PluginHandleQuery() {
    }

    public PluginHandleQuery(final String pluginId, final String field, final Integer type, final PageParameter pageParameter) {
        this.pluginId = pluginId;
        this.field = field;
        this.type = type;
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
     * Gets the value of field.
     *
     * @return the value of field
     */
    public String getField() {
        return field;
    }

    /**
     * Sets the field.
     *
     * @param field field
     */
    public void setField(final String field) {
        this.field = field;
    }

    /**
     * Gets the value of type.
     *
     * @return the value of type
     */
    public Integer getType() {
        return type;
    }

    /**
     * Sets the type.
     *
     * @param type type
     */
    public void setType(final Integer type) {
        this.type = type;
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

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static PluginHandleQuery.PluginHandleQueryBuilder builder() {
        return new PluginHandleQuery.PluginHandleQueryBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof PluginHandleQuery)) {
            return false;
        }
        PluginHandleQuery that = (PluginHandleQuery) o;
        return Objects.equals(pluginId, that.pluginId) && Objects.equals(field, that.field) && Objects.equals(type, that.type) && Objects.equals(pageParameter, that.pageParameter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(pluginId, field, type, pageParameter);
    }

    public static final class PluginHandleQueryBuilder {

        private String pluginId;

        private String field;

        private Integer type;

        private PageParameter pageParameter;

        private PluginHandleQueryBuilder() {
        }

        /**
         * pluginId.
         *
         * @param pluginId the pluginId.
         * @return PluginHandleQueryBuilder.
         */
        public PluginHandleQueryBuilder pluginId(final String pluginId) {
            this.pluginId = pluginId;
            return this;
        }

        /**
         * field.
         *
         * @param field the field.
         * @return PluginHandleQueryBuilder.
         */
        public PluginHandleQueryBuilder field(final String field) {
            this.field = field;
            return this;
        }

        /**
         * type.
         *
         * @param type the type.
         * @return PluginHandleQueryBuilder.
         */
        public PluginHandleQueryBuilder type(final Integer type) {
            this.type = type;
            return this;
        }

        /**
         * pageParameter.
         *
         * @param pageParameter the pageParameter.
         * @return PluginHandleQueryBuilder.
         */
        public PluginHandleQueryBuilder pageParameter(final PageParameter pageParameter) {
            this.pageParameter = pageParameter;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public PluginHandleQuery build() {
            PluginHandleQuery pluginHandleQuery = new PluginHandleQuery();
            pluginHandleQuery.setPluginId(pluginId);
            pluginHandleQuery.setField(field);
            pluginHandleQuery.setType(type);
            pluginHandleQuery.setPageParameter(pageParameter);
            return pluginHandleQuery;
        }
    }
}
