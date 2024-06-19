package org.apache.shenyu.admin.model.entity;

import java.sql.Timestamp;
import java.util.Objects;
/**
 * PluginNsRel do.
 */
public final class PluginNsRelDO extends BaseDO {

    /**
     *  namespaceId.
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

    public String getNamespaceId() {
        return namespaceId;
    }

    public void setNamespaceId(String namespaceId) {
        this.namespaceId = namespaceId;
    }

    public String getPluginId() {
        return pluginId;
    }

    public void setPluginId(String pluginId) {
        this.pluginId = pluginId;
    }

    public String getConfig() {
        return config;
    }

    public void setConfig(String config) {
        this.config = config;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    public Integer getSort() {
        return sort;
    }

    public void setSort(Integer sort) {
        this.sort = sort;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        PluginNsRelDO that = (PluginNsRelDO) o;
        return Objects.equals(namespaceId, that.namespaceId) && Objects.equals(pluginId, that.pluginId) && Objects.equals(config, that.config) && Objects.equals(enabled, that.enabled) && Objects.equals(sort, that.sort);
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

        public static PluginNsRelDOBuilder aPluginNsRelDO() {
            return new PluginNsRelDOBuilder();
        }

        public PluginNsRelDOBuilder id(String id) {
            this.id = id;
            return this;
        }

        public PluginNsRelDOBuilder dateCreated(Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        public PluginNsRelDOBuilder dateUpdated(Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        public PluginNsRelDOBuilder namespaceId(String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }

        public PluginNsRelDOBuilder pluginId(String pluginId) {
            this.pluginId = pluginId;
            return this;
        }

        public PluginNsRelDOBuilder config(String config) {
            this.config = config;
            return this;
        }

        public PluginNsRelDOBuilder enabled(Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        public PluginNsRelDOBuilder sort(Integer sort) {
            this.sort = sort;
            return this;
        }

        public PluginNsRelDO build() {
            PluginNsRelDO pluginNsRelDO = new PluginNsRelDO();
            pluginNsRelDO.setId(id);
            pluginNsRelDO.setDateCreated(dateCreated);
            pluginNsRelDO.setDateUpdated(dateUpdated);
            pluginNsRelDO.setNamespaceId(namespaceId);
            pluginNsRelDO.setPluginId(pluginId);
            pluginNsRelDO.setConfig(config);
            pluginNsRelDO.setEnabled(enabled);
            pluginNsRelDO.setSort(sort);
            return pluginNsRelDO;
        }
    }
}
