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

package org.apache.shenyu.agent.core.loader;

import org.apache.shenyu.agent.api.config.ShenyuAgentConfig;
import org.apache.shenyu.agent.core.locator.ShenyuAgentLocator;
import org.apache.shenyu.agent.core.yaml.ShenyuYamlEngine;

import java.io.File;
import java.io.IOException;

/**
 * The type Shenyu agent config loader.
 */
public final class ShenyuAgentConfigLoader {
    
    private static final String DEFAULT_CONFIG_PATH = "/conf/shenyu-agent.yaml";
    
    private static final String SPECIFIED_CONFIG_PATH = "config-path";
    
    /**
     * Load shenyu agent config.
     *
     * @return the shenyu agent config
     * @throws IOException the io exception
     */
    public static ShenyuAgentConfig load() throws IOException {
        String specifiedConfigPath = System.getProperty(SPECIFIED_CONFIG_PATH);
        File configFile = null == specifiedConfigPath ? new File(ShenyuAgentLocator.locatorAgent(), DEFAULT_CONFIG_PATH) : new File(specifiedConfigPath);
        return ShenyuYamlEngine.unmarshal(configFile, ShenyuAgentConfig.class);
    }
}
