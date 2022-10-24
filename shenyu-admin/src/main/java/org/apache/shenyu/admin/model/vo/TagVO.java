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
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.common.utils.DateUtils;

/**
 * this is tag view to web front.
 */
public class TagVO implements Serializable {

    private static final long serialVersionUID = 7020289373830629333L;

    /**
     * primary key.
     */
    private String id;

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
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    private Boolean hasChildren = false;

    public TagVO() {

    }

    public TagVO(final String id,
                 final String name,
                 final String tagDesc,
                 final String parentTagId,
                 final String ext,
                 final String dateCreated,
                 final String dateUpdated) {
        this.id = id;
        this.name = name;
        this.tagDesc = tagDesc;
        this.parentTagId = parentTagId;
        this.ext = ext;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
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
     * get name.
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * set name.
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * getTagDesc.
     * @return getTagDesc
     */
    public String getTagDesc() {
        return tagDesc;
    }

    /**
     * set tagDesc.
     * @param tagDesc tagDesc
     */
    public void setTagDesc(final String tagDesc) {
        this.tagDesc = tagDesc;
    }

    /**
     * get parentTagId.
     * @return parentTagId
     */
    public String getParentTagId() {
        return parentTagId;
    }

    /**
     * set parentTagId.
     * @param parentTagId parentTagId
     */
    public void setParentTagId(final String parentTagId) {
        this.parentTagId = parentTagId;
    }

    /**
     * get ext info.
     * @return extinfo
     */
    public String getExt() {
        return ext;
    }

    /**
     * set extinfo.
     * @param ext ext
     */
    public void setExt(final String ext) {
        this.ext = ext;
    }

    /**
     * get create time.
     * @return createtime
     */
    public String getDateCreated() {
        return dateCreated;
    }

    /**
     * set create time.
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final String dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * get update time.
     * @return updatetime
     */
    public String getDateUpdated() {
        return dateUpdated;
    }

    /**
     * set update time.
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final String dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    /**
     * judge tag whether in the bottom.
     * @return status
     */
    public Boolean getHasChildren() {
        return hasChildren;
    }

    /**
     * set status.
     * @param hasChildren status
     */
    public void setHasChildren(final Boolean hasChildren) {
        this.hasChildren = hasChildren;
    }

    /**
     * build tagVO.
     * @param tagDO tagDO
     * @return tagVO
     */
    public static TagVO buildTagVO(final TagDO tagDO) {
        return new TagVO(tagDO.getId(), tagDO.getName(), tagDO.getTagDesc(), tagDO.getParentTagId(), tagDO.getExt(),
                DateUtils.localDateTimeToString(tagDO.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(tagDO.getDateUpdated().toLocalDateTime()));
    }
}
