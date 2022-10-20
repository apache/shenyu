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

package org.apache.shenyu.admin.model.vo;

import java.io.Serializable;
import org.apache.shenyu.admin.model.entity.TagRelationDO;
import org.apache.shenyu.common.utils.DateUtils;

/**
 * this is tag relation view to web front.
 */
public class TagRelationVO implements Serializable {

    private static final long serialVersionUID = 197407943479009029L;

    /**
     * primary key.
     */
    private String id;

    /**
     * apiId.
     */
    private String apiId;

    /**
     * tagId.
     */
    private String tagId;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    public TagRelationVO() {

    }

    public TagRelationVO(final String id,
                         final String apiId,
                         final String tagId,
                         final String dateCreated,
                         final String dateUpdated) {
        this.id = id;
        this.apiId = apiId;
        this.tagId = tagId;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
    }

    /**
     * get primary key.
     *
     * @return primarykey
     */
    public String getId() {
        return id;
    }

    /**
     * set primary key.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * get apiId.
     *
     * @return apiId
     */
    public String getApiId() {
        return apiId;
    }

    /**
     * set apiId.
     *
     * @param apiId apiId
     */
    public void setApiId(final String apiId) {
        this.apiId = apiId;
    }

    /**
     * get tagId.
     *
     * @return tagId
     */
    public String getTagId() {
        return tagId;
    }

    /**
     * set tagId.
     *
     * @param tagId tagId
     */
    public void setTagId(final String tagId) {
        this.tagId = tagId;
    }

    /**
     * get createtime.
     *
     * @return createtime
     */
    public String getDateCreated() {
        return dateCreated;
    }

    /**
     * set createtime.
     *
     * @param dateCreated createtime
     */
    public void setDateCreated(final String dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * get updatetime.
     *
     * @return updatetime
     */
    public String getDateUpdated() {
        return dateUpdated;
    }

    /**
     * set updatetime.
     *
     * @param dateUpdated updatetime
     */
    public void setDateUpdated(final String dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    /**
     * build tagRelationVo.
     *
     * @param tagRelationDO tagRelationDO
     * @return TagRelationVO
     */
    public static TagRelationVO buildTagRelationVO(final TagRelationDO tagRelationDO) {
        return new TagRelationVO(tagRelationDO.getId(), tagRelationDO.getApiId(), tagRelationDO.getTagId(),
                DateUtils.localDateTimeToString(tagRelationDO.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(tagRelationDO.getDateUpdated().toLocalDateTime()));
    }
}
