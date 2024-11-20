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

import org.apache.shenyu.admin.model.dto.NamespacePluginDTO;
import org.apache.shenyu.common.utils.UUIDUtils;
import java.sql.Timestamp;
import java.util.Objects;
import java.util.Optional;

/**
 * NamespacePluginRel do.
 */
public final class NamespacePluginRelDO extends BaseDO {

    /**
     * namespaceId.
     */
    private String namespaceId;

    /**
     * plugin id.
     */
    private String pluginId;


    /**
     * plugin config @see 2.0.
     */
    private String config;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * plugin sort.
     */
    private Integer sort;

    /**
     * Gets the value of name.
     *
     * @return the value of name
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
     * Gets the value of name.
     *
     * @return the value of name
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
        NamespacePluginRelDO that = (NamespacePluginRelDO) o;
        return Objects.equals(namespaceId, that.namespaceId)
                && Objects.equals(pluginId, that.pluginId)
                && Objects.equals(config, that.config)
                && Objects.equals(enabled, that.enabled)
                && Objects.equals(sort, that.sort);
    }

    /**
     * build pluginDO.
     *
     * @param namespacePluginDTO {@linkplain NamespacePluginDTO}
     * @return {@linkplain NamespacePluginRelDO}
     */
    public static NamespacePluginRelDO buildNamespacePluginRelDO(final NamespacePluginDTO namespacePluginDTO) {
        return Optional.ofNullable(namespacePluginDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            return NamespacePluginRelDO.builder()
                    .id(item.getId())
                    .config(item.getConfig())
                    .enabled(item.getEnabled())
                    .sort(item.getSort())
                    .namespaceId(item.getNamespaceId())
                    .pluginId(item.getPluginId())
                    .dateUpdated(currentTime)
                    .build();
        }).orElse(null);
    }

    /**
     * build buildNamespacePluginRelDO.
     *
     * @param pluginDO {@linkplain PluginDO}
     * @param namespaceId {@linkplain String}
     * @return {@linkplain NamespacePluginRelDO}
     */
    public static NamespacePluginRelDO buildNamespacePluginRelDO(final PluginDO pluginDO, final String namespaceId) {
        return Optional.ofNullable(pluginDO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            return NamespacePluginRelDO.builder()
                    .id(UUIDUtils.getInstance().generateShortUuid())
                    .config(item.getConfig())
                    .enabled(item.getEnabled())
                    .sort(item.getSort())
                    .namespaceId(namespaceId)
                    .pluginId(item.getId())
                    .dateUpdated(currentTime)
                    .dateCreated(currentTime)
                    .build();
        }).orElse(null);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), namespaceId, pluginId, config, enabled, sort);
    }

    /**
     * builder.
     *
     * @return PluginNsRelDOBuilder
     */
    public static PluginNsRelDOBuilder builder() {
        return new PluginNsRelDOBuilder();
    }

    public static final class PluginNsRelDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String namespaceId;

        private String pluginId;

        private String config;

        private Boolean enabled;

        private Integer sort;

        private PluginNsRelDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return PluginNsRelDOBuilder.
         */
        public PluginNsRelDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return PluginNsRelDOBuilder.
         */
        public PluginNsRelDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return PluginNsRelDOBuilder.
         */
        public PluginNsRelDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * namespaceId.
         *
         * @param namespaceId the namespaceId.
         * @return PluginNsRelDOBuilder.
         */
        public PluginNsRelDOBuilder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }

        /**
         * pluginId.
         *
         * @param pluginId the pluginId.
         * @return PluginNsRelDOBuilder.
         */
        public PluginNsRelDOBuilder pluginId(final String pluginId) {
            this.pluginId = pluginId;
            return this;
        }

        /**
         * config.
         *
         * @param config the config.
         * @return PluginNsRelDOBuilder.
         */
        public PluginNsRelDOBuilder config(final String config) {
            this.config = config;
            return this;
        }

        /**
         * enabled.
         *
         * @param enabled the enabled.
         * @return PluginNsRelDOBuilder.
         */
        public PluginNsRelDOBuilder enabled(final Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * sort.
         *
         * @param sort the sort.
         * @return PluginNsRelDOBuilder.
         */
        public PluginNsRelDOBuilder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public NamespacePluginRelDO build() {
            NamespacePluginRelDO namespacePluginRelDO = new NamespacePluginRelDO();
            namespacePluginRelDO.setId(id);
            namespacePluginRelDO.setDateCreated(dateCreated);
            namespacePluginRelDO.setDateUpdated(dateUpdated);
            namespacePluginRelDO.setNamespaceId(namespaceId);
            namespacePluginRelDO.setPluginId(pluginId);
            namespacePluginRelDO.setConfig(config);
            namespacePluginRelDO.setEnabled(enabled);
            namespacePluginRelDO.setSort(sort);
            return namespacePluginRelDO;
        }
    }
}
