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

package org.apache.shenyu.register.client.polaris;

import com.tencent.polaris.api.plugin.configuration.ConfigFileResponse;
import com.tencent.polaris.configuration.api.core.ConfigFile;
import com.tencent.polaris.configuration.api.core.ConfigFileMetadata;
import com.tencent.polaris.configuration.api.core.ConfigFilePublishService;

import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.api.core.ConfigKVFile;
import java.util.Map;

public class PolarisMockConfigService implements ConfigFilePublishService, ConfigFileService {

    private final Map<ConfigFileMetadata, String> store;

    public PolarisMockConfigService(final Map<ConfigFileMetadata, String> store) {
        this.store = store;
    }

    @Override
    public ConfigFileResponse createConfigFile(final String s, final String s1, final String s2, final String s3) {
        return null;
    }

    @Override
    public ConfigFileResponse createConfigFile(final ConfigFileMetadata configFileMetadata, final String content) {
        store.put(configFileMetadata, content);
        return null;
    }

    @Override
    public ConfigFileResponse updateConfigFile(final String s, final String s1, final String s2, final String s3) {
        return null;
    }

    @Override
    public ConfigFileResponse updateConfigFile(final ConfigFileMetadata configFileMetadata, final String content) {
        store.put(configFileMetadata, content);
        return null;
    }

    @Override
    public ConfigFileResponse releaseConfigFile(final String s, final String s1, final String s2) {
        return null;
    }

    @Override
    public ConfigFileResponse releaseConfigFile(final ConfigFileMetadata configFileMetadata) {
        return null;
    }

    @Override
    public ConfigKVFile getConfigPropertiesFile(final String s, final String s1, final String s2) {
        return null;
    }

    @Override
    public ConfigKVFile getConfigPropertiesFile(final ConfigFileMetadata configFileMetadata) {
        store.get(configFileMetadata);
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
        return null;
    }

    @Override
    public ConfigFile getConfigFile(final ConfigFileMetadata configFileMetadata) {
        return new PolarisMockConfigFile(configFileMetadata, store.get(configFileMetadata));
    }
}
