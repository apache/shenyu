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

package org.apache.shenyu.discovery.api.config;

import java.util.Properties;

/**
 * The type Discovery config.
 */
public class DiscoveryConfig {
    
    private String name;
    
    private String type;
    
    private String serverList;
    
    private Properties props = new Properties();
    
    /**
     * Gets name.
     *
     * @return the name
     */
    public String getName() {
        return name;
    }
    
    /**
     * Sets name.
     *
     * @param name the name
     */
    public void setName(final String name) {
        this.name = name;
    }
    
    /**
     * Gets type.
     *
     * @return the type
     */
    public String getType() {
        return type;
    }
    
    /**
     * Sets type.
     *
     * @param type the type
     */
    public void setType(final String type) {
        this.type = type;
    }
    
    /**
     * Gets server list.
     *
     * @return the server list
     */
    public String getServerList() {
        return serverList;
    }
    
    /**
     * Sets server list.
     *
     * @param serverList the server list
     */
    public void setServerList(final String serverList) {
        this.serverList = serverList;
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
