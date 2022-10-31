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

package org.apache.shenyu.admin.model.bean;

import org.apache.commons.lang3.StringUtils;

import java.util.Objects;

/**
 * UpstreamInstance.
 */
public class UpstreamInstance {

    private String contextPath;

    private String ip;

    private int port;

    private Long startupTime;

    /**
     * instance health status.
     */
    private boolean healthy = true;

    /**
     * If instance is enabled to accept request.
     */
    private boolean enabled = true;

    /**
     * getClusterName.
     * @return String
     */
    public String getClusterName() {
        if (StringUtils.isNotEmpty(contextPath)) {
            return contextPath.substring(1);
        }
        return null;
    }

    /**
     * getContextPath.
     *
     * @return String
     */
    public String getContextPath() {
        return contextPath;
    }

    /**
     * setContextPath.
     *
     * @param contextPath contextPath
     */
    public void setContextPath(final String contextPath) {
        this.contextPath = contextPath;
    }

    /**
     * getIp.
     *
     * @return String
     */
    public String getIp() {
        return ip;
    }

    /**
     * setIp.
     *
     * @param ip ip
     */
    public void setIp(final String ip) {
        this.ip = ip;
    }

    /**
     * getPort.
     *
     * @return int
     */
    public int getPort() {
        return port;
    }

    /**
     * setPort.
     *
     * @param port port
     */
    public void setPort(final int port) {
        this.port = port;
    }

    /**
     * getStartupTime.
     *
     * @return Long
     */
    public Long getStartupTime() {
        return startupTime;
    }

    /**
     * setStartupTime.
     *
     * @param startupTime startupTime
     */
    public void setStartupTime(final Long startupTime) {
        this.startupTime = startupTime;
    }

    /**
     * isHealthy.
     * @return boolean
     */
    public boolean isHealthy() {
        return healthy;
    }

    /**
     * setHealthy.
     * @param healthy healthy
     */
    public void setHealthy(final boolean healthy) {
        this.healthy = healthy;
    }

    /**
     * isEnabled.
     * @return boolean
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * setEnabled.
     * @param enabled enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        UpstreamInstance that = (UpstreamInstance) o;
        return port == that.port
                && healthy == that.healthy
                && enabled == that.enabled
                && Objects.equals(contextPath, that.contextPath)
                && Objects.equals(ip, that.ip)
                && Objects.equals(startupTime, that.startupTime);
    }

    @Override
    public int hashCode() {
        return Objects.hash(contextPath, ip, port, startupTime, healthy, enabled);
    }
}
