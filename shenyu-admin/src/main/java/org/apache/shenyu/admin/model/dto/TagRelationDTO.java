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

package org.apache.shenyu.admin.model.dto;

import java.io.Serializable;
import org.apache.shenyu.admin.mapper.TagRelationMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

/**
 * this tag relation from web front.
 */
public class TagRelationDTO implements Serializable {

    private static final long serialVersionUID = -8300093387770924248L;

    /**
     * primary key.
     */
    @Existed(provider = TagRelationMapper.class, nullOfIgnore = true, message = "tag relation id is not existed")
    private String id;

    /**
     * apiId.
     */
    private String apiId;

    /**
     * tagId.
     */
    private String tagId;

    public TagRelationDTO() {

    }

    public TagRelationDTO(final String id, final String apiId, final String tagId) {
        this.id = id;
        this.apiId = apiId;
        this.tagId = tagId;
    }

    /**
     * get primary key.
     * @return primary key
     */
    public String getId() {
        return id;
    }

    /**
     * set primary key.
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * get api id.
     * @return apiId
     */
    public String getApiId() {
        return apiId;
    }

    /**
     * set apiId.
     * @param apiId apiid
     */
    public void setApiId(final String apiId) {
        this.apiId = apiId;
    }

    /**
     * get tag id.
     * @return tagId
     */
    public String getTagId() {
        return tagId;
    }

    /**
     *  set tag id.
     * @param tagId tagId
     */
    public void setTagId(final String tagId) {
        this.tagId = tagId;
    }

}
