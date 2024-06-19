package org.apache.shenyu.admin.model.entity;

import java.sql.Timestamp;
import java.util.Objects;

/**
 * Namespace do.
 */
public class NamespaceDO extends BaseDO {

    /**
     * the model namespaceId.
     */
    private String namespaceId;

    /**
     * the model name.
     */
    private String name;

    /**
     * the model desc.
     */
    private String description;

    public String getNamespaceId() {
        return namespaceId;
    }

    public void setNamespaceId(String namespaceId) {
        this.namespaceId = namespaceId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * builder.
     *
     * @return discoveryDOBuilder
     */
    public static NamespaceDOBuilder builder() {
        return new NamespaceDO.NamespaceDOBuilder();
    }


    public static final class NamespaceDOBuilder {
        private String id;
        private Timestamp dateCreated;
        private Timestamp dateUpdated;
        private String namespaceId;
        private String name;
        private String description;

        private NamespaceDOBuilder() {
        }

        public static NamespaceDOBuilder aNamespaceDO() {
            return new NamespaceDOBuilder();
        }

        public NamespaceDOBuilder id(String id) {
            this.id = id;
            return this;
        }

        public NamespaceDOBuilder dateCreated(Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        public NamespaceDOBuilder dateUpdated(Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        public NamespaceDOBuilder namespaceId(String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }

        public NamespaceDOBuilder name(String name) {
            this.name = name;
            return this;
        }

        public NamespaceDOBuilder description(String description) {
            this.description = description;
            return this;
        }

        public NamespaceDO build() {
            NamespaceDO namespaceDO = new NamespaceDO();
            namespaceDO.setId(id);
            namespaceDO.setDateCreated(dateCreated);
            namespaceDO.setDateUpdated(dateUpdated);
            namespaceDO.setNamespaceId(namespaceId);
            namespaceDO.setName(name);
            namespaceDO.setDescription(description);
            return namespaceDO;
        }
    }
}
