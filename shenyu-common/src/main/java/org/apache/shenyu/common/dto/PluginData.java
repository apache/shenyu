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

import java.util.Objects;

/**
 * PluginData.
 *
 * @since 2.0.0
 */
public class PluginData {

    private String id;

    private String name;

    private String config;

    private String role;

    private Boolean enabled;
    
    private Integer sort;

    /**
     * no args constructor.
     */
    public PluginData() {
    }

    /**
     * all args constructor without sort.
     *
     * @param id      id
     * @param name    name
     * @param config  config
     * @param role    role
     * @param enabled enabled
     */
    public PluginData(final String id, final String name, final String config, final String role, final Boolean enabled) {
        this.id = id;
        this.name = name;
        this.config = config;
        this.role = role;
        this.enabled = enabled;
    }
    
    /**
     * all args constructor.
     * 
     * @param id id
     * @param name name
     * @param config config
     * @param role role
     * @param enabled enabled
     * @param sort sort
     */
    public PluginData(final String id, final String name, final String config, final String role, final Boolean enabled,
                      final Integer sort) {
        this.id = id;
        this.name = name;
        this.config = config;
        this.role = role;
        this.enabled = enabled;
        this.sort = sort;
    }

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private PluginData(final Builder builder) {
        this.id = builder.id;
        this.name = builder.name;
        this.config = builder.config;
        this.role = builder.role;
        this.enabled = builder.enabled;
        this.sort = builder.sort;
    }

    /**
     * class builder.
     *
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
    }

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
     */
    public void setId(final String id) {
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
     * set name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get config.
     *
     * @return config
     */
    public String getConfig() {
        return config;
    }

    /**
     * set config.
     *
     * @param config config
     */
    public void setConfig(final String config) {
        this.config = config;
    }

    /**
     * get role.
     *
     * @return role
     */
    public String getRole() {
        return role;
    }

    /**
     * set role.
     *
     * @param role role
     */
    public void setRole(final String role) {
        this.role = role;
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
     * get sort.
     *
     * @return enabled
     */
    public Integer getSort() {
        return sort;
    }


    /**
     * set sort.
     * 
     * @param sort sort value
     */
    public void setSort(final Integer sort) {
        this.sort = sort;
    }

    /**
     * set enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        PluginData that = (PluginData) o;
        return Objects.equals(id, that.id) && Objects.equals(name, that.name) && Objects.equals(config, that.config)
                && Objects.equals(role, that.role) && Objects.equals(enabled, that.enabled);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, name, config, role, enabled);
    }

    @Override
    public String toString() {
        return "PluginData{"
                + "id='"
                + id
                + '\''
                + ", name='"
                + name
                + '\''
                + ", config='"
                + config
                + '\''
                + ", role='"
                + role
                + '\''
                + ", enabled="
                + enabled
                + '}';
    }

    /**
     * class builder.
     */
    public static final class Builder {

        /**
         * id.
         */
        private String id;

        /**
         * name.
         */
        private String name;

        /**
         * config.
         */
        private String config;

        /**
         * role.
         */
        private String role;

        /**
         * enabled.
         */
        private Boolean enabled;

        /**
         * sort.
         */
        private Integer sort;

        /**
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return PluginData
         */
        public PluginData build() {
            return new PluginData(this);
        }

        /**
         * build id.
         *
         * @param id id
         * @return this
         */
        public Builder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * build name.
         *
         * @param name name
         * @return this
         */
        public Builder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * build config.
         *
         * @param config config
         * @return this
         */
        public Builder config(final String config) {
            this.config = config;
            return this;
        }

        /**
         * build role.
         *
         * @param role role
         * @return this
         */
        public Builder role(final String role) {
            this.role = role;
            return this;
        }

        /**
         * build enabled.
         *
         * @param enabled enabled
         * @return this
         */
        public Builder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * build sort.
         *
         * @param sort sort
         * @return this
         */
        public Builder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }
    }
}
