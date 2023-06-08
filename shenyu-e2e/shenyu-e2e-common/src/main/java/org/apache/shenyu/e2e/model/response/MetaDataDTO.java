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
 * MetaDataDTO
 */
public class MetaDataDTO implements ResourceDTO {

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

    private MetaDataDTO(Builder builder) {
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

    public static Builder builder() {
        return new Builder();
    }

    public String getAppName() {
        return appName;
    }

    public void setAppName(String appName) {
        this.appName = appName;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getPathDesc() {
        return pathDesc;
    }

    public void setPathDesc(String pathDesc) {
        this.pathDesc = pathDesc;
    }

    public String getRpcType() {
        return rpcType;
    }

    public void setRpcType(String rpcType) {
        this.rpcType = rpcType;
    }

    public String getServiceName() {
        return serviceName;
    }

    public void setServiceName(String serviceName) {
        this.serviceName = serviceName;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public String getParameterTypes() {
        return parameterTypes;
    }

    public void setParameterTypes(String parameterTypes) {
        this.parameterTypes = parameterTypes;
    }

    public String getRpcExt() {
        return rpcExt;
    }

    public void setRpcExt(String rpcExt) {
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

    public void setId(String id) {
        this.id = id;
    }

    @Override
    public Date getDateCreated() {
        return dateCreated;
    }

    public void setDateCreated(Date dateCreated) {
        this.dateCreated = dateCreated;
    }

    @Override
    public Date getDateUpdated() {
        return dateUpdated;
    }

    public void setDateUpdated(Date dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
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

        public MetaDataDTO build() {
            return new MetaDataDTO(this);
        }

        public Builder appName(String appName) {
            this.appName = appName;
            return this;
        }

        public Builder path(String path) {
            this.path = path;
            return this;
        }

        public Builder pathDesc(String pathDesc) {
            this.pathDesc = pathDesc;
            return this;
        }

        public Builder rpcType(String rpcType) {
            this.rpcType = rpcType;
            return this;
        }

        public Builder serviceName(String serviceName) {
            this.serviceName = serviceName;
            return this;
        }

        public Builder methodName(String methodName) {
            this.methodName = methodName;
            return this;
        }

        public Builder parameterTypes(String parameterTypes) {
            this.parameterTypes = parameterTypes;
            return this;
        }

        public Builder rpcExt(String rpcExt) {
            this.rpcExt = rpcExt;
            return this;
        }

        public Builder id(String id) {
            this.id = id;
            return this;
        }

        public Builder dateCreated(Date dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        public Builder dateUpdated(Date dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        public Builder enabled(boolean enabled) {
            this.enabled = enabled;
            return this;
        }
    }
}
