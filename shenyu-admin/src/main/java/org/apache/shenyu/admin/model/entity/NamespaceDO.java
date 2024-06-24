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

import java.sql.Timestamp;

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

    /**
     * Gets the value of namespaceId.
     *
     * @return the value of namespaceId
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
     * Gets the value of description.
     *
     * @return the value of description
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the description.
     *
     * @param description description
     */
    public void setDescription(final String description) {
        this.description = description;
    }

    /**
     * builder.
     *
     * @return NamespaceDOBuilder
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

        /**
         * builder.
         *
         * @return NamespaceDO.NamespaceDOBuilder
         */
        public static NamespaceDOBuilder builder() {
            return new NamespaceDOBuilder();
        }

        /**
         * id.
         *
         * @param id the id.
         * @return NamespaceDOBuilder.
         */
        public NamespaceDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return NamespaceDOBuilder.
         */
        public NamespaceDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return NamespaceDOBuilder.
         */
        public NamespaceDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * namespaceId.
         *
         * @param namespaceId the namespaceId.
         * @return NamespaceDOBuilder.
         */
        public NamespaceDOBuilder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }

        /**
         * name.
         *
         * @param name the name.
         * @return NamespaceDOBuilder.
         */
        public NamespaceDOBuilder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * description.
         *
         * @param description the description.
         * @return NamespaceDOBuilder.
         */
        public NamespaceDOBuilder description(final String description) {
            this.description = description;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
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
