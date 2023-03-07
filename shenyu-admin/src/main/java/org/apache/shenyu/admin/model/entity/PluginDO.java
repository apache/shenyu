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

package org.apache.shenyu.admin.model.entity;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.io.IOException;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;

/**
 * The config field has been added in 2.0
 * PluginDO.
 */
public final class PluginDO extends BaseDO {

    private static final long serialVersionUID = -3414676617520629553L;

    /**
     * plugin name.
     */
    private String name;

    /**
     * plugin config @see 2.0.
     */
    private String config;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * the role to classify plugin.
     */
    private String role;

    /**
     * plugin sort.
     */
    private Integer sort;

    private byte[] pluginJar;

    public PluginDO() {
    }

    public PluginDO(final String name, final String config, final Boolean enabled, final String role, final Integer sort, final byte[] pluginJar) {
        this.name = name;
        this.config = config;
        this.enabled = enabled;
        this.role = role;
        this.sort = sort;
        this.pluginJar = pluginJar;
    }

    /**
     * Gets the value of pluginJar.
     *
     * @return the value of pluginJar
     */
    public byte[] getPluginJar() {
        return pluginJar;
    }

    /**
     * Sets the pluginJar.
     *
     * @param pluginJar pluginJar
     */
    public void setPluginJar(final byte[] pluginJar) {
        this.pluginJar = pluginJar;
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
     * Gets the value of config.
     *
     * @return the value of config
     */
    public String getConfig() {
        return config;
    }

    /**
     * Sets the config.
     *
     * @param config config
     */
    public void setConfig(final String config) {
        this.config = config;
    }

    /**
     * Gets the value of enabled.
     *
     * @return the value of enabled
     */
    public Boolean getEnabled() {
        return enabled;
    }

    /**
     * Sets the enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Gets the value of role.
     *
     * @return the value of role
     */
    public String getRole() {
        return role;
    }

    /**
     * Sets the role.
     *
     * @param role role
     */
    public void setRole(final String role) {
        this.role = role;
    }

    /**
     * Gets the value of sort.
     *
     * @return the value of sort
     */
    public Integer getSort() {
        return sort;
    }

    /**
     * Sets the sort.
     *
     * @param sort sort
     */
    public void setSort(final Integer sort) {
        this.sort = sort;
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static PluginDOBuilder builder() {
        return new PluginDOBuilder();
    }

    /**
     * build pluginDO.
     *
     * @param pluginDTO {@linkplain PluginDTO}
     * @return {@linkplain PluginDO}
     */
    public static PluginDO buildPluginDO(final PluginDTO pluginDTO) {
        return Optional.ofNullable(pluginDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            PluginDO pluginDO = PluginDO.builder()
                    .name(item.getName())
                    .config(item.getConfig())
                    .enabled(item.getEnabled())
                    .role(item.getRole())
                    .sort(item.getSort())
                    .dateUpdated(currentTime)
                    .build();

            if (StringUtils.isEmpty(item.getId())) {
                pluginDO.setId(UUIDUtils.getInstance().generateShortUuid());
                pluginDO.setDateCreated(currentTime);
            } else {
                pluginDO.setId(item.getId());
            }
            if (Objects.nonNull(item.getFile())) {
                try {
                    pluginDO.setPluginJar(item.getFile().getBytes());
                } catch (IOException e) {
                    throw new ShenyuException(e);
                }

            }
            return pluginDO;
        }).orElse(null);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        PluginDO pluginDO = (PluginDO) o;
        return Objects.equals(name, pluginDO.name)
                && Objects.equals(config, pluginDO.config)
                && Objects.equals(enabled, pluginDO.enabled)
                && Objects.equals(role, pluginDO.role)
                && Objects.equals(sort, pluginDO.sort)
                && Arrays.equals(pluginJar, pluginDO.pluginJar);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), name, config, enabled, role, sort);
    }

    public static final class PluginDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String name;

        private String config;

        private Boolean enabled;

        private String role;

        private Integer sort;

        private byte[] pluginJar;

        private PluginDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return PluginDOBuilder.
         */
        public PluginDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * id.
         *
         * @param dateCreated the dateCreated.
         * @return PluginDOBuilder.
         */
        public PluginDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return PluginDOBuilder.
         */
        public PluginDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * name.
         *
         * @param name the name.
         * @return PluginDOBuilder.
         */
        public PluginDOBuilder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * config.
         *
         * @param config the config.
         * @return PluginDOBuilder.
         */
        public PluginDOBuilder config(final String config) {
            this.config = config;
            return this;
        }

        /**
         * enabled.
         *
         * @param enabled the enabled.
         * @return PluginDOBuilder.
         */
        public PluginDOBuilder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * role.
         *
         * @param role the role.
         * @return PluginDOBuilder.
         */
        public PluginDOBuilder role(final String role) {
            this.role = role;
            return this;
        }

        /**
         * sort.
         *
         * @param sort the sort.
         * @return PluginDOBuilder.
         */
        public PluginDOBuilder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }

        /**
         * pluginJar.
         *
         * @param pluginJar  the  pluginJar.
         * @return PluginDOBuilder.
         */
        public PluginDOBuilder pluginJar(final byte[] pluginJar) {
            this.pluginJar = pluginJar;
            return this;
        }



        /**
         * build method.
         *
         * @return build object.
         */
        public PluginDO build() {
            PluginDO pluginDO = new PluginDO();
            pluginDO.setId(id);
            pluginDO.setDateCreated(dateCreated);
            pluginDO.setDateUpdated(dateUpdated);
            pluginDO.setName(name);
            pluginDO.setConfig(config);
            pluginDO.setEnabled(enabled);
            pluginDO.setRole(role);
            pluginDO.setSort(sort);
            pluginDO.setPluginJar(pluginJar);
            return pluginDO;
        }
    }
}
