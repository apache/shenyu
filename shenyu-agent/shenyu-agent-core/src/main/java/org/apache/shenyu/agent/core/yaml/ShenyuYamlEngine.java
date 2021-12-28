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

package org.apache.shenyu.agent.core.yaml;

import org.apache.shenyu.agent.api.config.AgentPluginConfig;
import org.apache.shenyu.agent.api.config.ShenyuAgentConfig;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.TypeDescription;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;
import org.yaml.snakeyaml.representer.Representer;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Map;

/**
 * The type Shenyu yaml engine.
 */
public final class ShenyuYamlEngine {
    
    private static final DumperOptions DUMPER_OPTIONS = new DumperOptions();
    
    static {
        DUMPER_OPTIONS.setLineBreak(DumperOptions.LineBreak.getPlatformLineBreak());
    }
    
    /**
     * Unmarshal t.
     *
     * @param <T> the type parameter
     * @param yamlFile the yaml file
     * @param classType the class type
     * @return the t
     * @throws IOException the io exception
     */
    public static <T> T unmarshal(final File yamlFile, final Class<T> classType) throws IOException {
        try (
                FileInputStream fileInputStream = new FileInputStream(yamlFile);
                InputStreamReader inputStreamReader = new InputStreamReader(fileInputStream)
        ) {
            return new Yaml(DUMPER_OPTIONS).loadAs(inputStreamReader, classType);
        }
    }
    
    /**
     * Agent config shenyu agent config.
     *
     * @param yamlFile the yaml file
     * @return the shenyu agent config
     * @throws IOException the io exception
     */
    public static ShenyuAgentConfig agentConfig(final File yamlFile) throws IOException {
        try (
                FileInputStream fileInputStream = new FileInputStream(yamlFile);
                InputStreamReader inputStreamReader = new InputStreamReader(fileInputStream)
        ) {
            Constructor constructor = new Constructor(ShenyuAgentConfig.class);
            TypeDescription customTypeDescription = new TypeDescription(AgentPluginConfig.class);
            customTypeDescription.addPropertyParameters("plugins", Map.class);
            constructor.addTypeDescription(customTypeDescription);
            return new Yaml(constructor, new Representer(DUMPER_OPTIONS)).loadAs(inputStreamReader, ShenyuAgentConfig.class);
        }
    }
}
