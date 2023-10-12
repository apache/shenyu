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

import com.tencent.polaris.api.exception.ServerCodes;
import com.tencent.polaris.api.plugin.configuration.ConfigFile;
import com.tencent.polaris.api.plugin.configuration.ConfigFileResponse;
import com.tencent.polaris.configuration.api.core.ConfigFileMetadata;
import com.tencent.polaris.configuration.api.core.ConfigFilePublishService;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.api.core.ConfigKVFile;
import com.tencent.polaris.configuration.client.internal.DefaultConfigFileMetadata;
import java.util.Map;

public class PolarisMockConfigService implements ConfigFilePublishService, ConfigFileService {

    private final Map<ConfigFileMetadata, String> store;

    public PolarisMockConfigService(final Map<ConfigFileMetadata, String> store) {
        this.store = store;
    }

    @Override
    public ConfigFileResponse createConfigFile(final String namespace, final String fileGroup, final String fileName, final String content) {
        final DefaultConfigFileMetadata configFileMetadata = new DefaultConfigFileMetadata(namespace, fileGroup, fileName);
        store.put(configFileMetadata, content);
        final ConfigFile configFile = new ConfigFile(namespace, fileGroup, fileName);
        configFile.setContent(content);
        return new ConfigFileResponse(ServerCodes.EXECUTE_SUCCESS, "success", configFile);
    }

    @Override
    public ConfigFileResponse createConfigFile(final ConfigFileMetadata configFileMetadata, final String content) {
        return createConfigFile(configFileMetadata.getNamespace(), configFileMetadata.getFileGroup(), configFileMetadata.getFileName(), content);
    }

    @Override
    public ConfigFileResponse updateConfigFile(final String namespace, final String fileGroup, final String fileName, final String content) {
        final DefaultConfigFileMetadata configFileMetadata = new DefaultConfigFileMetadata(namespace, fileGroup, fileName);
        store.put(configFileMetadata, content);
        final ConfigFile configFile =
            new ConfigFile(namespace, fileGroup, fileName);
        configFile.setContent(content);
        return new ConfigFileResponse(ServerCodes.EXECUTE_SUCCESS, "success", configFile);
    }

    @Override
    public ConfigFileResponse updateConfigFile(final ConfigFileMetadata configFileMetadata, final String content) {
        return updateConfigFile(configFileMetadata.getNamespace(), configFileMetadata.getFileGroup(), configFileMetadata.getFileName(), content);
    }

    @Override
    public ConfigFileResponse releaseConfigFile(final String namespace, final String fileGroup, final String fileName) {
        final DefaultConfigFileMetadata configFileMetadata = new DefaultConfigFileMetadata(namespace, fileGroup, fileName);
        final ConfigFile configFile = new ConfigFile(namespace, fileGroup, fileName);
        configFile.setContent(store.getOrDefault(configFileMetadata, "{}"));
        return new ConfigFileResponse(ServerCodes.EXECUTE_SUCCESS, "success", configFile);
    }

    @Override
    public ConfigFileResponse releaseConfigFile(final ConfigFileMetadata configFileMetadata) {
        return releaseConfigFile(configFileMetadata.getNamespace(), configFileMetadata.getFileGroup(), configFileMetadata.getFileName());
    }

    @Override
    public ConfigKVFile getConfigPropertiesFile(final String namespace, final String fileGroup, final String fileName) {
        final DefaultConfigFileMetadata configFileMetadata = new DefaultConfigFileMetadata(namespace, fileGroup, fileName);
        return new PolarisMockConfigFile(configFileMetadata, store.getOrDefault(configFileMetadata, "{}"));
    }

    @Override
    public ConfigKVFile getConfigPropertiesFile(final ConfigFileMetadata configFileMetadata) {
        return getConfigPropertiesFile(configFileMetadata.getNamespace(), configFileMetadata.getFileGroup(), configFileMetadata.getFileName());
    }

    @Override
    public ConfigKVFile getConfigYamlFile(final String namespace, final String fileGroup, final String fileName) {
        final DefaultConfigFileMetadata configFileMetadata = new DefaultConfigFileMetadata(namespace, fileGroup, fileName);
        return new PolarisMockConfigFile(configFileMetadata, store.getOrDefault(configFileMetadata, "{}"));
    }

    @Override
    public ConfigKVFile getConfigYamlFile(final ConfigFileMetadata configFileMetadata) {
        return getConfigYamlFile(configFileMetadata.getNamespace(), configFileMetadata.getFileGroup(), configFileMetadata.getFileName());
    }

    @Override
    public com.tencent.polaris.configuration.api.core.ConfigFile getConfigFile(final String namespace, final String fileGroup, final String fileName) {
        return getConfigFile(new DefaultConfigFileMetadata(namespace, fileGroup, fileName));
    }

    @Override
    public com.tencent.polaris.configuration.api.core.ConfigFile getConfigFile(final ConfigFileMetadata configFileMetadata) {
        return new PolarisMockConfigFile(configFileMetadata, store.getOrDefault(configFileMetadata, null));
    }
}
