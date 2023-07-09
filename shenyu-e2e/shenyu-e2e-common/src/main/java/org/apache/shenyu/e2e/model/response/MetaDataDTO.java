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

package org.apache.shenyu.e2e.model.response;

import com.fasterxml.jackson.annotation.JsonFormat;

import java.util.Date;

/**
 * MetaDataDTO.
 */
public final class MetaDataDTO implements ResourceDTO {

    /**
     * appName.
     */
    private String appName;

    /**
     * the path.
     */
    private String path;

    /**
     * path desc.
     */
    private String pathDesc;

    /**
     * rpc type.
     */
    private String rpcType;

    /**
     * service name.
     */
    private String serviceName;

    /**
     * method name.
     */
    private String methodName;

    /**
     * parameter typs.
     */
    private String parameterTypes;

    /**
     * rpc ext.
     */
    private String rpcExt;

    private String id;

    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date dateCreated;

    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date dateUpdated;

    private boolean enabled;

    private MetaDataDTO() {
    }

    private MetaDataDTO(final Builder builder) {
        this.appName = builder.appName;
        this.path = builder.path;
        this.pathDesc = builder.pathDesc;
        this.rpcType = builder.rpcType;
        this.serviceName = builder.serviceName;
        this.methodName = builder.methodName;
        this.parameterTypes = builder.parameterTypes;
        this.rpcExt = builder.rpcExt;
        this.id = builder.id;
        this.dateCreated = builder.dateCreated;
        this.dateUpdated = builder.dateUpdated;
        this.enabled = builder.enabled;
    }
    
    /**
     * builder.
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
    }
    
    /**
     * get app name.
     * @return String
     */
    public String getAppName() {
        return this.appName;
    }
    
    /**
     * app name.
     * @param appName appName
     */
    public void setAppName(final String appName) {
        this.appName = appName;
    }
    
    /**
     * path.
     * @return String
     */
    public String getPath() {
        return path;
    }
    
    /**
     * set path.
     * @param path path
     */
    public void setPath(final String path) {
        this.path = path;
    }
    
    /**
     * path describe.
     * @return String
     */
    public String getPathDesc() {
        return pathDesc;
    }
    
    /**
     * path describe.
     * @param pathDesc pathDesc
     */
    public void setPathDesc(final String pathDesc) {
        this.pathDesc = pathDesc;
    }
    
    /**
     * rpc type.
     * @return String
     */
    public String getRpcType() {
        return rpcType;
    }
    
    /**
     * rpc type.
     * @param rpcType rpcType
     */
    public void setRpcType(final String rpcType) {
        this.rpcType = rpcType;
    }
    
    /**
     * service name.
     * @return String
     */
    public String getServiceName() {
        return serviceName;
    }
    
    /**
     * service name.
     * @param serviceName serviceName
     */
    public void setServiceName(final String serviceName) {
        this.serviceName = serviceName;
    }
    
    /**
     * method name.
     * @return String
     */
    public String getMethodName() {
        return methodName;
    }
    
    /**
     * method name.
     * @param methodName methodName
     */
    public void setMethodName(final String methodName) {
        this.methodName = methodName;
    }
    
    /**
     * parameter types.
     * @return String
     */
    public String getParameterTypes() {
        return parameterTypes;
    }
    
    /**
     * parameter types.
     * @param parameterTypes parameterTypes
     */
    public void setParameterTypes(final String parameterTypes) {
        this.parameterTypes = parameterTypes;
    }
    
    /**
     * rpc extends.
     * @return String
     */
    public String getRpcExt() {
        return rpcExt;
    }
    
    /**
     * rpc extends.
     * @param rpcExt rpcExt
     */
    public void setRpcExt(final String rpcExt) {
        this.rpcExt = rpcExt;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public String getName() {
        return null;
    }
    
    /**
     * id.
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    @Override
    public Date getDateCreated() {
        return dateCreated;
    }
    
    /**
     * create date.
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Date dateCreated) {
        this.dateCreated = dateCreated;
    }

    @Override
    public Date getDateUpdated() {
        return dateUpdated;
    }
    
    /**
     * update date.
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Date dateUpdated) {
        this.dateUpdated = dateUpdated;
    }
    
    /**
     * enabled.
     * @return boolean
     */
    public boolean isEnabled() {
        return enabled;
    }
    
    /**
     * enabled.
     * @param enabled enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    public static final class Builder {
        /**
         * appName.
         */
        private String appName;

        /**
         * the path.
         */
        private String path;

        /**
         * path desc.
         */
        private String pathDesc;

        /**
         * rpc type.
         */
        private String rpcType;

        /**
         * service name.
         */
        private String serviceName;

        /**
         * method name.
         */
        private String methodName;

        /**
         * parameter typs.
         */
        private String parameterTypes;

        /**
         * rpc ext.
         */
        private String rpcExt;

        private String id;

        @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
        private Date dateCreated;

        @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
        private Date dateUpdated;

        private boolean enabled;

        private Builder() {

        }
        
        /**
         * build.
         * @return MetaDataDTO
         */
        public MetaDataDTO build() {
            return new MetaDataDTO(this);
        }
        
        /**
         * app name.
         * @param appName appName
         * @return Builder
         */
        public Builder appName(final String appName) {
            this.appName = appName;
            return this;
        }
        
        /**
         * path.
         * @param path path
         * @return Builder
         */
        public Builder path(final String path) {
            this.path = path;
            return this;
        }
        
        /**
         * path describe.
         * @param pathDesc pathDesc
         * @return Builder
         */
        public Builder pathDesc(final String pathDesc) {
            this.pathDesc = pathDesc;
            return this;
        }
        
        /**
         * rpc type.
         * @param rpcType rpcType
         * @return Builder
         */
        public Builder rpcType(final String rpcType) {
            this.rpcType = rpcType;
            return this;
        }
        
        /**
         * service name.
         * @param serviceName serviceName
         * @return Builder
         */
        public Builder serviceName(final String serviceName) {
            this.serviceName = serviceName;
            return this;
        }
        
        /**
         * method name.
         * @param methodName methodName
         * @return Builder
         */
        public Builder methodName(final String methodName) {
            this.methodName = methodName;
            return this;
        }
        
        /**
         * parameter types.
         * @param parameterTypes parameterTypes
         * @return Builder
         */
        public Builder parameterTypes(final String parameterTypes) {
            this.parameterTypes = parameterTypes;
            return this;
        }
        
        /**
         * rpc extends.
         * @param rpcExt rpcExt
         * @return Builder
         */
        public Builder rpcExt(final String rpcExt) {
            this.rpcExt = rpcExt;
            return this;
        }
        
        /**
         * id.
         * @param id id
         * @return Builder
         */
        public Builder id(final String id) {
            this.id = id;
            return this;
        }
        
        /**
         * create date.
         * @param dateCreated dateCreated
         * @return Builder
         */
        public Builder dateCreated(final Date dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }
        
        /**
         * update date.
         * @param dateUpdated dateUpdated
         * @return Builder
         */
        public Builder dateUpdated(final Date dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }
        
        /**
         * enabled.
         * @param enabled enabled
         * @return Builder
         */
        public Builder enabled(final boolean enabled) {
            this.enabled = enabled;
            return this;
        }
    }
}
