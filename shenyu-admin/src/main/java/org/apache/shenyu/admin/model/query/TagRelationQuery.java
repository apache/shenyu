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

import java.util.Objects;

/**
 * this is tag relation query.
 */
public class TagRelationQuery {

    /**
     * api id.
     */
    private String apiId;

    /**
     * tag id.
     */
    private String tagId;

    /**
     * get api id.
     * @return get api id
     */
    public String getApiId() {
        return apiId;
    }

    /**
     * set api id.
     * @param apiId api id
     */
    public void setApiId(final String apiId) {
        this.apiId = apiId;
    }

    /**
     * get tag id.
     * @return tagid
     */
    public String getTagId() {
        return tagId;
    }

    /**
     * set tag id.
     * @param tagId tagid
     */
    public void setTagId(final String tagId) {
        this.tagId = tagId;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof TagRelationQuery)) {
            return false;
        }
        TagRelationQuery that = (TagRelationQuery) o;
        return Objects.equals(apiId, that.apiId)
                && Objects.equals(tagId, that.tagId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(apiId, tagId);
    }

    /**
     * builder.
     * @return TagRelationQueryBuilder
     */
    public static TagRelationQueryBuilder builder() {
        return new TagRelationQueryBuilder();
    }

    public static final class TagRelationQueryBuilder {

        /**
         * apiId.
         */
        private String apiId;

        /**
         * tagId.
         */
        private String tagId;

        private TagRelationQueryBuilder() {
        }

        /**
         * build apiId.
         * @param apiId apiId
         * @return TagRelationQueryBuilder
         */
        public TagRelationQueryBuilder apiId(final String apiId) {
            this.apiId = apiId;
            return this;
        }

        /**
         * build tagId.
         * @param tagId tagId
         * @return TagRelationQueryBuilder
         */
        public TagRelationQueryBuilder tagId(final String tagId) {
            this.tagId = tagId;
            return this;
        }

        /**
         * build.
         * @return TagRelationQuery
         */
        public TagRelationQuery build() {
            TagRelationQuery tagRelationQuery = new TagRelationQuery();
            tagRelationQuery.setApiId(apiId);
            tagRelationQuery.setTagId(tagId);
            return tagRelationQuery;
        }
    }
}
