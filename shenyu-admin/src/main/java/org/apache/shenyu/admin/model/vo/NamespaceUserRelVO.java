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

/**
 * this is namespace user view to web front.
 */
public class NamespaceUserRelVO implements Serializable {
    
    private static final long serialVersionUID = -7590798095079377811L;
    
    /**
     * id.
     */
    private String id;
    
    /**
     * user id.
     */
    private String userId;
    
    /**
     * namespace id.
     */
    private String namespaceId;
    
    
    /**
     * created time.
     */
    private String dateCreated;
    
    /**
     * updated time.
     */
    private String dateUpdated;
    
    
    public NamespaceUserRelVO() {
    }
    
    public NamespaceUserRelVO(final String id, final String pluginId, final String namespaceId, final String dateCreated, final String dateUpdated) {
        this.id = id;
        this.userId = pluginId;
        this.namespaceId = namespaceId;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
    }
    
    /**
     * Gets the value of id.
     *
     * @return the value of id
     */
    public String getId() {
        return id;
    }
    
    /**
     * Sets the id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
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
     * Gets the namespace id.
     *
     * @return the plugin handle list
     */
    public String getNamespaceId() {
        return namespaceId;
    }
    
    /**
     * set namespace id.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }
    
    /**
     * Gets the value of dateCreated.
     *
     * @return the value of dateCreated
     */
    public String getDateCreated() {
        return dateCreated;
    }
    
    /**
     * Sets the dateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final String dateCreated) {
        this.dateCreated = dateCreated;
    }
    
    /**
     * Gets the value of dateUpdated.
     *
     * @return the value of dateUpdated
     */
    public String getDateUpdated() {
        return dateUpdated;
    }
    
    /**
     * Sets the dateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final String dateUpdated) {
        this.dateUpdated = dateUpdated;
    }
    
    
    public static NamespaceUserVOBuilder builder() {
        return new NamespaceUserVOBuilder();
    }
    
    public static final class NamespaceUserVOBuilder {
        
        /**
         * id.
         */
        private String id;
        
        /**
         * user id.
         */
        private String userId;
        
        /**
         * namespace id.
         */
        private String namespaceId;
        
        /**
         * created time.
         */
        private String dateCreated;
        
        /**
         * updated time.
         */
        private String dateUpdated;
        
        private NamespaceUserVOBuilder() {
        }
        
        public static NamespaceUserVOBuilder builder() {
            return new NamespaceUserVOBuilder();
        }
        
        /**
         * id.
         *
         * @param id the id
         * @return NamespaceUserVOBuilder
         */
        public NamespaceUserVOBuilder id(final String id) {
            this.id = id;
            return this;
        }
        
        /**
         * user id.
         *
         * @param userId the userId
         * @return NamespaceUserVOBuilder
         */
        public NamespaceUserVOBuilder userId(final String userId) {
            this.userId = userId;
            return this;
        }
        
        /**
         * namespace id.
         *
         * @param namespaceId the namespaceId
         * @return NamespaceUserVOBuilder
         */
        public NamespaceUserVOBuilder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }
        
        /**
         * created time.
         *
         * @param dateCreated the dateCreated
         * @return NamespaceUserVOBuilder
         */
        public NamespaceUserVOBuilder dateCreated(final String dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }
        
        /**
         * updated time.
         *
         * @param dateUpdated the dateUpdated
         * @return NamespaceUserVOBuilder
         */
        public NamespaceUserVOBuilder dateUpdated(final String dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }
        
        public NamespaceUserRelVO build() {
            return new NamespaceUserRelVO(this.id, this.userId, this.namespaceId, this.dateCreated, this.dateUpdated);
        }
    }
    
    
}
