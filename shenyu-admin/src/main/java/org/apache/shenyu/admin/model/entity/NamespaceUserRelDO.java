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

import java.util.Objects;

/**
 * NamespaceUserRelDO.
 */
public final class NamespaceUserRelDO extends BaseDO {
    
    /**
     * namespaceId.
     */
    private String namespaceId;
    
    /**
     * userId.
     */
    private String userId;
    
    /**
     * Gets the value of namespaceId.
     *
     * @return the value of namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }
    
    /**
     * Sets the namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }
    
    /**
     * Gets the value of userId.
     *
     * @return the value of userId
     */
    public String getUserId() {
        return userId;
    }
    
    /**
     * Sets the userId.
     *
     * @param userId userId
     */
    public void setUserId(final String userId) {
        this.userId = userId;
    }
    
    /**
     * builder method.
     *
     * @return builder object.
     */
    public static NamespaceUserRelDOBuilder builder() {
        return new NamespaceUserRelDOBuilder();
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        NamespaceUserRelDO that = (NamespaceUserRelDO) o;
        return namespaceId.equals(that.namespaceId) && userId.equals(that.userId);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(namespaceId, userId);
    }
    
    @Override
    public String toString() {
        return "NamespaceUserRelDO{" + "namespaceId='" + namespaceId + '\'' + ", userId='" + userId + '\'' + '}';
    }
    
    
    public static final class NamespaceUserRelDOBuilder {
        
        /**
         * id.
         */
        private String id;
        
        /**
         * namespaceId.
         */
        private String namespaceId;
        
        /**
         * userId.
         */
        private String userId;
        
        private NamespaceUserRelDOBuilder() {
        }
        
        public static NamespaceUserRelDOBuilder builder() {
            return new NamespaceUserRelDOBuilder();
        }
        
        /**
         * Sets the id.
         *
         * @param id id
         * @return NamespaceUserRelDOBuilder
         */
        public NamespaceUserRelDOBuilder id(final String id) {
            this.id = id;
            return this;
        }
        
        /**
         * Sets the namespaceId.
         *
         * @param namespaceId namespaceId
         * @return NamespaceUserRelDOBuilder
         */
        public NamespaceUserRelDOBuilder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }
        
        /**
         * Sets the userId.
         *
         * @param userId userId
         * @return NamespaceUserRelDOBuilder
         */
        public NamespaceUserRelDOBuilder userId(final String userId) {
            this.userId = userId;
            return this;
        }
        
        public NamespaceUserRelDO build() {
            NamespaceUserRelDO namespaceUserRelDO = new NamespaceUserRelDO();
            namespaceUserRelDO.setId(id);
            namespaceUserRelDO.setNamespaceId(namespaceId);
            namespaceUserRelDO.setUserId(userId);
            return namespaceUserRelDO;
        }
    }
    
}
