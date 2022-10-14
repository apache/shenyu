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
import java.util.Objects;

public final class TagDO extends BaseDO {

    private static final long serialVersionUID = -3968123108441095604L;

    /**
     * name.
     */
    private String name;

    /**
     * tagDesc.
     */
    private String tagDesc;

    /**
     * parentTagId.
     */
    private String parentTagId;

    /**
     * ext.
     */
    private String ext;

    /**
     * getName.
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
     * getTagDesc.
     *
     * @return tagDesc
     */
    public String getTagDesc() {
        return tagDesc;
    }

    /**
     * setTagDesc.
     *
     * @param tagDesc tagDesc
     */
    public void setTagDesc(final String tagDesc) {
        this.tagDesc = tagDesc;
    }

    /**
     * getParentId.
     *
     * @return parentTagId
     */
    public String getParentTagId() {
        return parentTagId;
    }

    /**
     * setParentId.
     *
     * @param parentTagId parentId
     */
    public void setParentTagId(final String parentTagId) {
        this.parentTagId = parentTagId;
    }

    /**
     * getExt.
     *
     * @return ext
     */
    public String getExt() {
        return ext;
    }

    /**
     * setExt.
     *
     * @param ext ext
     */
    public void setExt(final String ext) {
        this.ext = ext;
    }

    public static TagDO.TagDOBuilder builder() {
        return new TagDO.TagDOBuilder();
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
        TagDO tagDO = (TagDO) o;
        return Objects.equals(name, tagDO.name)
                && Objects.equals(tagDesc, tagDO.tagDesc)
                && Objects.equals(ext, tagDO.ext)
                && Objects.equals(parentTagId, tagDO.parentTagId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), name, tagDesc, ext, parentTagId);
    }

    public static final class TagDOBuilder {

        private String id;

        private String name;

        private String tagDesc;

        private String parentTagId;

        private String ext;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private TagDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return RuleDOBuilder.
         */
        public TagDO.TagDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return RuleDOBuilder.
         */
        public TagDO.TagDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return RuleDOBuilder.
         */
        public TagDO.TagDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * tagDesc.
         *
         * @param tagDesc tagDesc.
         * @return TagDOBuilder.
         */
        public TagDO.TagDOBuilder tagDesc(final String tagDesc) {
            this.tagDesc = tagDesc;
            return this;
        }

        /**
         * ext.
         *
         * @param ext ext.
         * @return TagDOBuilder.
         */
        public TagDO.TagDOBuilder ext(final String ext) {
            this.ext = ext;
            return this;
        }

        /**
         * parentTagId.
         *
         * @param parentTagId parentTagId.
         * @return TagDOBuilder.
         */
        public TagDO.TagDOBuilder parentTagId(final String parentTagId) {
            this.parentTagId = parentTagId;
            return this;
        }

        /**
         * name.
         *
         * @param name name
         * @return TagDOBuilder.
         */
        public TagDO.TagDOBuilder name(final String name) {
            this.name = name;
            return this;
        }

        public TagDO build() {
            TagDO tagDO = new TagDO();
            tagDO.setExt(ext);
            tagDO.setTagDesc(tagDesc);
            tagDO.setParentTagId(parentTagId);
            tagDO.setName(name);
            tagDO.setId(id);
            tagDO.setDateCreated(dateCreated);
            tagDO.setDateUpdated(dateUpdated);
            return tagDO;
        }
    }

}
