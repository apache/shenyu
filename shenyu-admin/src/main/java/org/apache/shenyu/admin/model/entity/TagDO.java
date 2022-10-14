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
     * @return tagDesc
     */
    public String getTagDesc() {
        return tagDesc;
    }

    /**
     * setTagDesc.
     * @param tagDesc tagDesc
     */
    public void setTagDesc(final String tagDesc) {
        this.tagDesc = tagDesc;
    }

    /**
     * getParentId.
     * @return parentTagId
     */
    public String getParentTagId() {
        return parentTagId;
    }

    /**
     * setParentId.
     * @param parentTagId parentId
     */
    public void setParentTagId(final String parentTagId) {
        this.parentTagId = parentTagId;
    }

    /**
     * getExt.
     * @return ext
     */
    public String getExt() {
        return ext;
    }

    /**
     * setExt.
     * @param ext ext
     */
    public void setExt(final String ext) {
        this.ext = ext;
    }

}
