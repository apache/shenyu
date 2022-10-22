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

package org.apache.shenyu.register.instance.api.config;

import java.util.Properties;

/**
 * The type Register config.
 */
public class RegisterConfig {
    
    private boolean enabled;
    
    private String registerType;
    
    private String serverLists;
    
    private Properties props = new Properties();
    
    /**
     * RegisterConfig.
     */
    public RegisterConfig() {
        
    }
    
    /**
     * registerType.
     *
     * @param registerType the register type
     * @param serverLists the server lists
     * @param props the props
     */
    public RegisterConfig(final String registerType, final String serverLists, final Properties props) {
        this.registerType = registerType;
        this.serverLists = serverLists;
        this.props = props;
    }
    
    /**
     * getRegisterType.
     *
     * @return String register type
     */
    public String getRegisterType() {
        return registerType;
    }
    
    /**
     * setRegisterType.
     *
     * @param registerType registerType
     */
    public void setRegisterType(final String registerType) {
        this.registerType = registerType;
    }
    
    /**
     * getServerLists.
     *
     * @return String server lists
     */
    public String getServerLists() {
        return serverLists;
    }
    
    /**
     * setServerLists.
     *
     * @param serverLists serverLists
     */
    public void setServerLists(final String serverLists) {
        this.serverLists = serverLists;
    }
    
    /**
     * getProps.
     *
     * @return String props
     */
    public Properties getProps() {
        return props;
    }
    
    /**
     * setProps.
     *
     * @param props props
     */
    public void setProps(final Properties props) {
        this.props = props;
    }
    
    /**
     * Gets enabled.
     *
     * @return the enabled
     */
    public boolean getEnabled() {
        return enabled;
    }
    
    /**
     * Sets enabled.
     *
     * @param enabled the enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }
}
