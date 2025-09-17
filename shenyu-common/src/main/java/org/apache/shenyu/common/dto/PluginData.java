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

    private String pluginJar;

    private String namespaceId;

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
    public PluginData(final String id, final String name, final String config, final String role, final Boolean enabled, final String pluginJar) {
        this.id = id;
        this.name = name;
        this.config = config;
        this.role = role;
        this.enabled = enabled;
        this.pluginJar = pluginJar;
    }

    /**
     * all args constructor.
     *
     * @param id      id
     * @param name    name
     * @param config  config
     * @param role    role
     * @param enabled enabled
     * @param sort    sort
     */
    public PluginData(final String id, final String name, final String config, final String role, final Boolean enabled,
                      final Integer sort, final String pluginJar) {
        this.id = id;
        this.name = name;
        this.config = config;
        this.role = role;
        this.enabled = enabled;
        this.sort = sort;
        this.pluginJar = pluginJar;
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
        this.pluginJar = builder.pluginJar;
    }

    /**
     * all args constructor.
     *
     * @param id      id
     * @param name    name
     * @param config  config
     * @param role    role
     * @param enabled enabled
     * @param sort    sort
     */
    public PluginData(final String id, final String name, final String config, final String role, final Boolean enabled, final Integer sort, final String pluginJar, final String namespaceId) {
        this.id = id;
        this.name = name;
        this.config = config;
        this.role = role;
        this.enabled = enabled;
        this.sort = sort;
        this.pluginJar = pluginJar;
        this.namespaceId = namespaceId;
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
     * get pluginJar.
     *
     * @return pluginJar
     */
    public String getPluginJar() {
        return pluginJar;
    }

    /**
     * set pluginJar.
     *
     * @param pluginJar pluginJar
     */
    public void setPluginJar(final String pluginJar) {
        this.pluginJar = pluginJar;
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
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        PluginData that = (PluginData) o;
        return Objects.equals(id, that.id) && Objects.equals(name, that.name) && Objects.equals(config, that.config)
                && Objects.equals(role, that.role) && Objects.equals(enabled, that.enabled) && Objects.equals(sort, that.sort)
                && Objects.equals(pluginJar, that.pluginJar) && Objects.equals(namespaceId, that.namespaceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, name, config, role, enabled, sort, pluginJar, namespaceId);
    }

    public static final class Builder {

        private String id;

        private String name;

        private String config;

        private String role;

        private Boolean enabled;

        private Integer sort;

        private String pluginJar;

        private String namespaceId;

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
        public static Builder builder() {
            return new Builder();
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

        /**
         * build pluginJar.
         *
         * @param pluginJar pluginJar
         * @return this
         */
        public Builder pluginJar(final String pluginJar) {
            this.pluginJar = pluginJar;
            return this;
        }

        /**
         * build namespaceId.
         *
         * @param namespaceId namespaceId
         * @return this
         */
        public Builder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }

        /**
         * build new Object.
         *
         * @return PluginData
         */
        public PluginData build() {
            PluginData pluginData = new PluginData();
            pluginData.setId(id);
            pluginData.setName(name);
            pluginData.setConfig(config);
            pluginData.setRole(role);
            pluginData.setEnabled(enabled);
            pluginData.setSort(sort);
            pluginData.setPluginJar(pluginJar);
            pluginData.setNamespaceId(namespaceId);
            return pluginData;
        }
    }
}
