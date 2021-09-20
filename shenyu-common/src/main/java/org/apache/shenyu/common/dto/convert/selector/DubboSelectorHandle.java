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

package org.apache.shenyu.common.dto.convert.selector;

import java.io.Serializable;
import java.util.Objects;

/**
 * The type Dubbo selector handle.
 */
public class DubboSelectorHandle implements Serializable {

    private static final long serialVersionUID = 8965968591478216223L;

    /**
     * zookeeper url is required.
     */
    private String registry;

    /**
     * dubbo application name is required.
     */
    private String appName;

    /**
     * dubbo protocol.
     */
    private String protocol;

    /**
     * port.
     */
    private int port;

    /**
     * upstreamUrl.
     */
    private String upstreamUrl;

    /**
     * gray status.
     */
    private Boolean gray;

    /**
     * group.
     */
    private String group;

    /**
     * version.
     */
    private String version;

    /**
     * weight.
     */
    private int weight;

    /**
     * false close/ true open.
     */
    private boolean status = true;

    /**
     * startup time.
     */
    private long timestamp;

    /**
     * warmup.
     */
    private int warmup;

    /**
     * get registry.
     *
     * @return registry
     */
    public String getRegistry() {
        return registry;
    }

    /**
     * set registry.
     *
     * @param registry registry
     */
    public void setRegistry(final String registry) {
        this.registry = registry;
    }

    /**
     * get appName.
     *
     * @return appName
     */
    public String getAppName() {
        return appName;
    }

    /**
     * set appName.
     *
     * @param appName appName
     */
    public void setAppName(final String appName) {
        this.appName = appName;
    }

    /**
     * get protocol.
     *
     * @return protocol
     */
    public String getProtocol() {
        return protocol;
    }

    /**
     * set protocol.
     *
     * @param protocol protocol
     */
    public void setProtocol(final String protocol) {
        this.protocol = protocol;
    }

    /**
     * get port.
     *
     * @return port
     */
    public int getPort() {
        return port;
    }

    /**
     * set port.
     *
     * @param port port
     */
    public void setPort(final int port) {
        this.port = port;
    }

    /**
     * Gets the value of gray.
     *
     * @return the value of gray
     */
    public Boolean isGray() {
        return gray;
    }

    /**
     * Sets the gray.
     *
     * @param gray gray
     */
    public void setGray(final Boolean gray) {
        this.gray = gray;
    }

    /**
     * Gets the value of upstreamUrl.
     *
     * @return the value of upstreamUrl
     */
    public String getUpstreamUrl() {
        return upstreamUrl;
    }

    /**
     * Sets the upstreamUrl.
     *
     * @param upstreamUrl upstreamUrl
     */
    public void setUpstreamUrl(final String upstreamUrl) {
        this.upstreamUrl = upstreamUrl;
    }

    /**
     * Gets the value of weight.
     *
     * @return the value of weight
     */
    public int getWeight() {
        return weight;
    }

    /**
     * Sets the weight.
     *
     * @param weight weight
     */
    public void setWeight(final int weight) {
        this.weight = weight;
    }

    /**
     * Gets the value of status.
     *
     * @return the value of status
     */
    public boolean isStatus() {
        return status;
    }

    /**
     * Sets the status.
     *
     * @param status status
     */
    public void setStatus(final boolean status) {
        this.status = status;
    }

    /**
     * Gets the value of timestamp.
     *
     * @return the value of timestamp
     */
    public long getTimestamp() {
        return timestamp;
    }

    /**
     * Sets the timestamp.
     *
     * @param timestamp timestamp
     */
    public void setTimestamp(final long timestamp) {
        this.timestamp = timestamp;
    }

    /**
     * Gets the value of warmup.
     *
     * @return the value of warmup
     */
    public int getWarmup() {
        return warmup;
    }

    /**
     * Sets the warmup.
     *
     * @param warmup warmup
     */
    public void setWarmup(final int warmup) {
        this.warmup = warmup;
    }

    /**
     * Gets the value of group.
     *
     * @return the value of group
     */
    public String getGroup() {
        return group;
    }

    /**
     * Sets the group.
     *
     * @param group group
     */
    public void setGroup(final String group) {
        this.group = group;
    }

    /**
     * Gets the value of version.
     *
     * @return the value of version
     */
    public String getVersion() {
        return version;
    }

    /**
     * Sets the version.
     *
     * @param version version
     */
    public void setVersion(final String version) {
        this.version = version;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof DubboSelectorHandle)) {
            return false;
        }
        DubboSelectorHandle that = (DubboSelectorHandle) o;
        return port == that.port
                && weight == that.weight
                && status == that.status
                && timestamp == that.timestamp
                && warmup == that.warmup
                && Objects.equals(registry, that.registry)
                && Objects.equals(appName, that.appName)
                && Objects.equals(protocol, that.protocol)
                && Objects.equals(upstreamUrl, that.upstreamUrl)
                && Objects.equals(gray, that.gray)
                && Objects.equals(group, that.group)
                && Objects.equals(version, that.version);
    }

    @Override
    public int hashCode() {
        return Objects.hash(registry, appName, protocol, port, upstreamUrl, gray, weight, status, timestamp, warmup, group, version);
    }

    @Override
    public String toString() {
        return "DubboSelectorHandle{"
                + "registry='" + registry
                + ", appName='" + appName
                + ", protocol='" + protocol
                + ", port=" + port
                + ", upstreamUrl='" + upstreamUrl
                + ", gray=" + gray
                + ", weight=" + weight
                + ", status=" + status
                + ", timestamp=" + timestamp
                + ", warmup=" + warmup
                + ", group='" + group
                + ", version='" + version
                + '}';
    }
}
