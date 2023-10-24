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
 * this is tag query.
 */
public class TagQuery {

    /**
     * name.
     */
    private String name;

    /**
     * parentTagId.
     */
    private String parentTagId;

    /**
     * get parent tag id.
     * @return parentTagId
     */
    public String getParentTagId() {
        return parentTagId;
    }

    /**
     * set parent tag id.
     * @param parentTagId parenttagid
     */
    public void setParentTagId(final String parentTagId) {
        this.parentTagId = parentTagId;
    }

    /**
     *  get name.
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

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof TagQuery)) {
            return false;
        }
        TagQuery that = (TagQuery) o;
        return Objects.equals(name, that.name)
                && Objects.equals(parentTagId, that.parentTagId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, parentTagId);
    }

}
