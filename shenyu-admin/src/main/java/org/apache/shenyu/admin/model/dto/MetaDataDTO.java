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

import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Objects;

public class MetaDataDTO implements Serializable {
    
    private static final long serialVersionUID = 7476312364813536366L;
    
    @Existed(provider = MetaDataMapper.class, nullOfIgnore = true, message = "meta data is not existed")
    private String id;
    
    /**
     * appName must is not null.
     */
    @NotBlank
    private String appName;
    
    /**
     * context path is not null.
     */
    private String contextPath;
    
    /**
     * the path is not null.
     */
    @NotBlank
    private String path;
    
    /**
     * rule name is not null.
     */
    private String ruleName;
    
    /**
     * path desc.
     */
    private String pathDesc;
    
    /**
     * rpc tyoe is not null.
     */
    @NotBlank
    private String rpcType;
    
    /**
     * service name is not null.
     */
    @NotBlank
    private String serviceName;
    
    /**
     * method name is not null.
     */
    @NotBlank
    private String methodName;
    
    /**
     * parameter typs.
     */
    private String parameterTypes;
    
    /**
     * rpc ext.
     */
    private String rpcExt;
    
    /**
     * whether enabled.
     */
    @NotNull
    private Boolean enabled;

    /**
     * namespaceId.
     */
    @NotBlank
    @Existed(message = "namespaceId is not existed", provider = NamespaceMapper.class)
    private String namespaceId;
    
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
     * Gets the value of appName.
     *
     * @return the value of appName
     */
    public String getAppName() {
        return appName;
    }
    
    /**
     * Sets the appName.
     *
     * @param appName appName
     */
    public void setAppName(final String appName) {
        this.appName = appName;
    }
    
    /**
     * Gets the value of contextPath.
     *
     * @return the value of contextPath
     */
    public String getContextPath() {
        return contextPath;
    }
    
    /**
     * Sets the contextPath.
     *
     * @param contextPath contextPath
     */
    public void setContextPath(final String contextPath) {
        this.contextPath = contextPath;
    }
    
    /**
     * Gets the value of path.
     *
     * @return the value of path
     */
    public String getPath() {
        return path;
    }
    
    /**
     * Sets the path.
     *
     * @param path path
     */
    public void setPath(final String path) {
        this.path = path;
    }
    
    /**
     * Gets the value of ruleName.
     *
     * @return the value of ruleName
     */
    public String getRuleName() {
        return ruleName;
    }
    
    /**
     * Sets the ruleName.
     *
     * @param ruleName ruleName
     */
    public void setRuleName(final String ruleName) {
        this.ruleName = ruleName;
    }
    
    /**
     * Gets the value of pathDesc.
     *
     * @return the value of pathDesc
     */
    public String getPathDesc() {
        return pathDesc;
    }
    
    /**
     * Sets the pathDesc.
     *
     * @param pathDesc pathDesc
     */
    public void setPathDesc(final String pathDesc) {
        this.pathDesc = pathDesc;
    }
    
    /**
     * Gets the value of rpcType.
     *
     * @return the value of rpcType
     */
    public String getRpcType() {
        return rpcType;
    }
    
    /**
     * Sets the rpcType.
     *
     * @param rpcType rpcType
     */
    public void setRpcType(final String rpcType) {
        this.rpcType = rpcType;
    }
    
    /**
     * Gets the value of serviceName.
     *
     * @return the value of serviceName
     */
    public String getServiceName() {
        return serviceName;
    }
    
    /**
     * Sets the serviceName.
     *
     * @param serviceName serviceName
     */
    public void setServiceName(final String serviceName) {
        this.serviceName = serviceName;
    }
    
    /**
     * Gets the value of methodName.
     *
     * @return the value of methodName
     */
    public String getMethodName() {
        return methodName;
    }
    
    /**
     * Sets the methodName.
     *
     * @param methodName methodName
     */
    public void setMethodName(final String methodName) {
        this.methodName = methodName;
    }
    
    /**
     * Gets the value of parameterTypes.
     *
     * @return the value of parameterTypes
     */
    public String getParameterTypes() {
        return parameterTypes;
    }
    
    /**
     * Sets the parameterTypes.
     *
     * @param parameterTypes parameterTypes
     */
    public void setParameterTypes(final String parameterTypes) {
        this.parameterTypes = parameterTypes;
    }
    
    /**
     * Gets the value of rpcExt.
     *
     * @return the value of rpcExt
     */
    public String getRpcExt() {
        return rpcExt;
    }
    
    /**
     * Sets the rpcExt.
     *
     * @param rpcExt rpcExt
     */
    public void setRpcExt(final String rpcExt) {
        this.rpcExt = rpcExt;
    }
    
    /**
     * Gets the value of enabled.
     *
     * @return the value of enabled
     */
    public Boolean getEnabled() {
        return enabled;
    }
    
    /**
     * Sets the enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * get namespaceId.
     *
     * @return namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * set namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        MetaDataDTO that = (MetaDataDTO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(appName, that.appName)
                && Objects.equals(contextPath, that.contextPath)
                && Objects.equals(path, that.path)
                && Objects.equals(ruleName, that.ruleName)
                && Objects.equals(pathDesc, that.pathDesc)
                && Objects.equals(rpcType, that.rpcType)
                && Objects.equals(serviceName, that.serviceName)
                && Objects.equals(methodName, that.methodName)
                && Objects.equals(parameterTypes, that.parameterTypes)
                && Objects.equals(rpcExt, that.rpcExt)
                && Objects.equals(enabled, that.enabled);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(id, appName, contextPath, path, ruleName, pathDesc, rpcType, serviceName, methodName, parameterTypes, rpcExt, enabled);
    }
}
