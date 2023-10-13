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

package org.apache.shenyu.e2e.config;

import org.apache.shenyu.e2e.enums.ServiceTypeEnum;

import java.util.Properties;

public class ServiceConfigure {
    
    private String serviceName;
    
    private String moduleName;
    
    private String baseUrl;
    
    private String schema;
    
    private Integer port;
    
    private ServiceTypeEnum serviceType;
    
    private Properties parameters;
    
    /**
     * get serviceName.
     *
     * @return serviceName
     */
    public String getServiceName() {
        return serviceName;
    }
    
    /**
     * set serviceName.
     * @param serviceName serviceName
     *
     */
    public void setServiceName(final String serviceName) {
        this.serviceName = serviceName;
    }
    
    /**
     * get moduleName.
     *
     * @return moduleName
     */
    public String getModuleName() {
        return moduleName;
    }
    
    /**
     * set moduleName.
     *
     * @param moduleName moduleName
     */
    public void setModuleName(final String moduleName) {
        this.moduleName = moduleName;
    }
    
    /**
     * get baseUrl.
     *
     * @return baseUrl
     */
    public String getBaseUrl() {
        return baseUrl;
    }
    
    /**
     * set baseUrl.
     *
     * @param baseUrl base url
     */
    public void setBaseUrl(final String baseUrl) {
        this.baseUrl = baseUrl;
    }
    
    /**
     * get schema.
     *
     * @return scheme
     */
    public String getSchema() {
        return schema;
    }
    
    /**
     * set schema.
     *
     * @param schema schema
     */
    public void setSchema(final String schema) {
        this.schema = schema;
    }
    
    /**
     * get port.
     *
     * @return port
     */
    public Integer getPort() {
        return port;
    }
    
    /**
     * set port.
     *
     * @param port port
     */
    public void setPort(final Integer port) {
        this.port = port;
    }
    
    /**
     * get serviceType.
     *
     * @return serviceType
     */
    public ServiceTypeEnum getServiceType() {
        return serviceType;
    }
    
    /**
     * set serviceType.
     *
     * @param serviceType service type
     */
    public void setServiceType(final ServiceTypeEnum serviceType) {
        this.serviceType = serviceType;
    }
    
    /**
     * get parameters.
     *
     * @return parameters
     */
    public Properties getParameters() {
        return parameters;
    }
    
    /**
     * set parameters.
     *
     * @param parameters parameters
     */
    public void setParameters(final Properties parameters) {
        this.parameters = parameters;
    }
}
