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

import java.io.Serializable;
import java.util.Objects;

/**
 * this is permission query.
 */
public class PermissionQuery implements Serializable {

    private static final long serialVersionUID = 6937903404332979859L;

    /**
     * object id : role id or user id.
     */
    private String objectId;

    /**
     * resource id.
     */
    private String resourceId;

    public PermissionQuery() {
    }

    public PermissionQuery(final String objectId, final String resourceId) {
        this.objectId = objectId;
        this.resourceId = resourceId;
    }

    /**
     * Gets the value of objectId.
     *
     * @return the value of objectId
     */
    public String getObjectId() {
        return objectId;
    }

    /**
     * Sets the objectId.
     *
     * @param objectId objectId
     */
    public void setObjectId(final String objectId) {
        this.objectId = objectId;
    }

    /**
     * Gets the value of resourceId.
     *
     * @return the value of resourceId
     */
    public String getResourceId() {
        return resourceId;
    }

    /**
     * Sets the resourceId.
     *
     * @param resourceId resourceId
     */
    public void setResourceId(final String resourceId) {
        this.resourceId = resourceId;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof PermissionQuery)) {
            return false;
        }
        PermissionQuery that = (PermissionQuery) o;
        return Objects.equals(objectId, that.objectId) && Objects.equals(resourceId, that.resourceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(objectId, resourceId);
    }
}
