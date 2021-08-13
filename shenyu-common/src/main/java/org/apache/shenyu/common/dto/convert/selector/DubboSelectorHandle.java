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

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        DubboSelectorHandle that = (DubboSelectorHandle) o;
        return port == that.port && Objects.equals(registry, that.registry)
                && Objects.equals(appName, that.appName) && Objects.equals(protocol, that.protocol);
    }

    @Override
    public int hashCode() {
        return Objects.hash(registry, appName, protocol, port);
    }

    @Override
    public String toString() {
        return "DubboSelectorHandle{"
                + "registry='"
                + registry
                + '\''
                + ", appName='"
                + appName
                + '\''
                + ", protocol='"
                + protocol
                + '\''
                + ", port="
                + port
                + '}';
    }
}
