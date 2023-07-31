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

package org.apache.shenyu.sync.data.polaris.handler;

import com.tencent.polaris.configuration.api.core.ConfigFile;
import com.tencent.polaris.configuration.api.core.ConfigFileMetadata;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.api.core.ConfigKVFile;

import java.util.Map;

public class PolarisMockConfigFileService implements ConfigFileService {

    private final Map<String, String> store;

    public PolarisMockConfigFileService(final Map<String, String> store) {
        this.store = store;
    }

    @Override
    public ConfigKVFile getConfigPropertiesFile(final String s, final String s1, final String s2) {
        return null;
    }

    @Override
    public ConfigKVFile getConfigPropertiesFile(final ConfigFileMetadata configFileMetadata) {
        return null;
    }

    @Override
    public ConfigKVFile getConfigYamlFile(final String s, final String s1, final String s2) {
        return null;
    }

    @Override
    public ConfigKVFile getConfigYamlFile(final ConfigFileMetadata configFileMetadata) {
        return null;
    }

    @Override
    public ConfigFile getConfigFile(final String s, final String s1, final String s2) {
        return new PolarisMockConfigFile(store.getOrDefault(s, "{}"));
    }

    @Override
    public ConfigFile getConfigFile(final ConfigFileMetadata configFileMetadata) {
        return null;
    }
}
