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
import org.apache.shenyu.admin.mapper.TagMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

/**
 * this tag from web front.
 */
public class TagDTO implements Serializable {

    private static final long serialVersionUID = 1680976393721978145L;

    /**
     * primary key.
     */
    /**
     * primary key.
     */
    @Existed(provider = TagMapper.class, nullOfIgnore = true, message = "tag is not existed")
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

    public TagDTO() {

    }

    public TagDTO(final String id, final String name, final String tagDesc, final String parentTagId, final String ext) {
        this.id = id;
        this.name = name;
        this.tagDesc = tagDesc;
        this.parentTagId = parentTagId;
    }

    /**
     * get primary key.
     *
     * @return primary key.
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
     *
     * @return getName
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
     * get tag description.
     *
     * @return tagDesc
     */
    public String getTagDesc() {
        return tagDesc;
    }

    /**
     * set tag description.
     *
     * @param tagDesc tagDesc
     */
    public void setTagDesc(final String tagDesc) {
        this.tagDesc = tagDesc;
    }

    /**
     * get parentTagId.
     *
     * @return getParentTagId
     */
    public String getParentTagId() {
        return parentTagId;
    }

    /**
     * set parentTagId.
     *
     * @param parentTagId parentTagId
     */
    public void setParentTagId(final String parentTagId) {
        this.parentTagId = parentTagId;
    }

}
