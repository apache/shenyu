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

package org.apache.shenyu.register.common.config;

import java.util.Properties;

/**
 * Governance center configuration.
 */
public final class ShenyuRegisterCenterConfig extends PropertiesConfig {
    
    private String registerType;
    
    private String serverLists;

    public ShenyuRegisterCenterConfig() {

    }

    public ShenyuRegisterCenterConfig(final String registerType,
                                      final String serverLists,
                                      final Properties props) {
        this.registerType = registerType;
        this.serverLists = serverLists;
        this.setProps(props);
    }

    /**
     * getRegisterType.
     *
     * @return String
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
     * @return String
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
}
