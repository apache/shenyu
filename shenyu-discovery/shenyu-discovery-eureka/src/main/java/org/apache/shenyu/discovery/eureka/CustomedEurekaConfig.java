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

package org.apache.shenyu.discovery.eureka;

import com.netflix.appinfo.EurekaInstanceConfig;
import com.netflix.appinfo.MyDataCenterInstanceConfig;
import org.apache.commons.lang.StringUtils;

import java.util.Map;

public class CustomedEurekaConfig extends MyDataCenterInstanceConfig implements EurekaInstanceConfig {

    private String applicationName;

    private String instanceId;

    private String ipAddress;

    private int port = -1;

    private Map<String, String> metadata;

    @Override
    public String getInstanceId() {
        if (StringUtils.isBlank(instanceId)) {
            return super.getInstanceId();
        }
        return instanceId;
    }

    @Override
    public String getIpAddress() {
        if (StringUtils.isBlank(ipAddress)) {
            return super.getIpAddress();
        }
        return ipAddress;
    }

    @Override
    public int getNonSecurePort() {
        if (port == -1) {
            return super.getNonSecurePort();
        }
        return port;
    }

    @Override
    public String getAppname() {
        if (StringUtils.isBlank(applicationName)) {
            return super.getAppname();
        }
        return applicationName;
    }

    @Override
    public String getHostName(final boolean refresh) {
        return this.getIpAddress();
    }

    @Override
    public Map<String, String> getMetadataMap() {
        return metadata;
    }

    /**
     * Sets the instance ID.
     *
     * @param instanceId The unique identifier for the instance.
     */
    public void setInstanceId(final String instanceId) {
        this.instanceId = instanceId;
    }

    /**
     * Sets the IP address.
     *
     * @param ipAddress The IP address of the instance.
     */
    public void setIpAddress(final String ipAddress) {
        this.ipAddress = ipAddress;
    }

    /**
     * Sets the port number.
     *
     * @param port The port number where the service is running.
     */
    public void setPort(final int port) {
        this.port = port;
    }

    /**
     * Sets the application name.
     *
     * @param applicationName The name of the application.
     */
    public void setApplicationName(final String applicationName) {
        this.applicationName = applicationName;
    }

    /**
     * Sets the application name.
     *
     * @param metadata The metadata of the instance
     */
    public void setMetadataMap(final Map<String, String> metadata) {
        this.metadata = metadata;
    }
}
