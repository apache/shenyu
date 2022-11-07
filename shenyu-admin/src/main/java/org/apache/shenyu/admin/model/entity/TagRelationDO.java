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
import java.util.Optional;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.TagRelationDTO;
import org.apache.shenyu.common.utils.UUIDUtils;

public final class TagRelationDO extends BaseDO {

    private static final long serialVersionUID = -2968123108441795604L;

    /**
     * appid.
     */
    private String apiId;

    /**
     * tagId.
     */
    private String tagId;

    /**
     * getApiId.
     *
     * @return apiId
     */
    public String getApiId() {
        return apiId;
    }

    /**
     * setApiId.
     *
     * @param apiId apiId
     */
    public void setApiId(final String apiId) {
        this.apiId = apiId;
    }

    /**
     * getTagId.
     *
     * @return tagId
     */
    public String getTagId() {
        return tagId;
    }

    /**
     * setTagId.
     *
     * @param tagId tagId
     */
    public void setTagId(final String tagId) {
        this.tagId = tagId;
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
        TagRelationDO tagRelationDO = (TagRelationDO) o;
        return Objects.equals(apiId, tagRelationDO.apiId)
                && Objects.equals(tagId, tagRelationDO.tagId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), apiId, tagId);
    }

    /**
     * build tagRelationDO.
     *
     * @param tagRelationDTO {@linkplain TagRelationDTO}
     * @return {@linkplain TagRelationDO}
     */
    public static TagRelationDO buildTagRelationDO(final TagRelationDTO tagRelationDTO) {
        return Optional.ofNullable(tagRelationDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            TagRelationDO tagRelationDO = TagRelationDO.builder()
                    .apiId(tagRelationDTO.getApiId())
                    .tagId(tagRelationDTO.getTagId())
                    .dateUpdated(currentTime)
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                tagRelationDO.setId(UUIDUtils.getInstance().generateShortUuid());
                tagRelationDO.setDateCreated(currentTime);
            } else {
                tagRelationDO.setId(item.getId());
            }
            return tagRelationDO;
        }).orElse(null);
    }

    /**
     * builder .
     *
     * @return tagRelationDO
     */
    public static TagRelationDO.TagRelationDOBuilder builder() {
        return new TagRelationDO.TagRelationDOBuilder();
    }

    public static final class TagRelationDOBuilder {

        private String id;

        private String apiId;

        private String tagId;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private TagRelationDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return RuleDOBuilder.
         */
        public TagRelationDO.TagRelationDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return RuleDOBuilder.
         */
        public TagRelationDO.TagRelationDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return RuleDOBuilder.
         */
        public TagRelationDO.TagRelationDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * tagId.
         *
         * @param tagId tagId.
         * @return TagDOBuilder.
         */
        public TagRelationDO.TagRelationDOBuilder tagId(final String tagId) {
            this.tagId = tagId;
            return this;
        }

        /**
         * ext.
         *
         * @param apiId apiId.
         * @return TagRelationDO.
         */
        public TagRelationDO.TagRelationDOBuilder apiId(final String apiId) {
            this.apiId = apiId;
            return this;
        }

        /**
         * build.
         *
         * @return TagRelationDO
         */
        public TagRelationDO build() {
            TagRelationDO tagRelationDO = new TagRelationDO();
            tagRelationDO.setApiId(apiId);
            tagRelationDO.setId(id);
            tagRelationDO.setTagId(tagId);
            tagRelationDO.setDateCreated(dateCreated);
            tagRelationDO.setDateUpdated(dateUpdated);
            return tagRelationDO;
        }
    }

}
