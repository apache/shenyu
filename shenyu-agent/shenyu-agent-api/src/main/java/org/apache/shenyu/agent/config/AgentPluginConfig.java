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

package org.apache.shenyu.agent.config;

import java.util.Properties;

/**
 * The type Agent plugin config.
 */
public final class AgentPluginConfig {
    
    private String host;
    
    private int port;
    
    private String password;
    
    private Properties props;
    
    /**
     * Instantiates a new Agent plugin config.
     *
     * @param host the host
     * @param port the port
     * @param password the password
     * @param props the props
     */
    public AgentPluginConfig(final String host, final int port, final String password, final Properties props) {
        this.host = host;
        this.port = port;
        this.password = password;
        this.props = props;
    }
    
    /**
     * Gets host.
     *
     * @return the host
     */
    public String getHost() {
        return host;
    }
    
    /**
     * Sets host.
     *
     * @param host the host
     */
    public void setHost(final String host) {
        this.host = host;
    }
    
    /**
     * Gets port.
     *
     * @return the port
     */
    public int getPort() {
        return port;
    }
    
    /**
     * Sets port.
     *
     * @param port the port
     */
    public void setPort(final int port) {
        this.port = port;
    }
    
    /**
     * Gets password.
     *
     * @return the password
     */
    public String getPassword() {
        return password;
    }
    
    /**
     * Sets password.
     *
     * @param password the password
     */
    public void setPassword(final String password) {
        this.password = password;
    }
    
    /**
     * Gets props.
     *
     * @return the props
     */
    public Properties getProps() {
        return props;
    }
    
    /**
     * Sets props.
     *
     * @param props the props
     */
    public void setProps(final Properties props) {
        this.props = props;
    }
}
