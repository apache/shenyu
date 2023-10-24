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

import com.google.gson.reflect.TypeToken;
import com.tencent.polaris.api.config.Configuration;
import com.tencent.polaris.api.core.ProviderAPI;
import com.tencent.polaris.api.rpc.InstanceRegisterRequest;
import com.tencent.polaris.client.api.SDKContext;
import com.tencent.polaris.configuration.api.core.ConfigFile;
import com.tencent.polaris.configuration.api.core.ConfigFileMetadata;
import com.tencent.polaris.configuration.api.core.ConfigFilePublishService;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.client.internal.DefaultConfigFileMetadata;
import com.tencent.polaris.configuration.factory.ConfigFileServiceFactory;
import com.tencent.polaris.configuration.factory.ConfigFileServicePublishFactory;
import com.tencent.polaris.factory.ConfigAPIFactory;
import com.tencent.polaris.factory.api.DiscoveryAPIFactory;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentLinkedDeque;
import static org.apache.shenyu.common.constant.Constants.NAMESPACE;
import org.apache.shenyu.common.constant.PolarisPathConstants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Polaris register center client.
 */
@Join
public class PolarisClientRegisterRepository implements ShenyuClientRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(PolarisClientRegisterRepository.class);

    private final ConcurrentLinkedDeque<MetaDataRegisterDTO> metaDataRegisterDTOList = new ConcurrentLinkedDeque<>();

    private ConfigFileService configFileService;

    private ConfigFilePublishService configFilePublishService;

    private ProviderAPI providerAPI;

    private Properties props;

    @Override
    public void init(final ShenyuRegisterCenterConfig config) {
        Configuration configuration = buildConfiguration(config);
        SDKContext sdkContext = SDKContext.initContextByConfig(configuration);
        this.providerAPI = DiscoveryAPIFactory.createProviderAPIByContext(sdkContext);
        this.configFileService = ConfigFileServiceFactory.createConfigFileService(sdkContext);
        this.configFilePublishService = ConfigFileServicePublishFactory.createConfigFilePublishService(sdkContext);
        this.props = config.getProps();
    }

    /**
     * build polaris configuration.
     *
     * @param config config
     * @return Configuration
     */
    private Configuration buildConfiguration(final ShenyuRegisterCenterConfig config) {
        String serverLists = config.getServerLists();
        return ConfigAPIFactory.createConfigurationByAddress(serverLists);
    }

    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        registerMetaData(metadata);
    }

    @Override
    public void persistURI(final URIRegisterDTO registerDTO) {
        registerUri(registerDTO);
    }

    private void registerUri(final URIRegisterDTO registerDTO) {
        final String namespace = props.getProperty(NAMESPACE, PolarisPathConstants.NAMESPACE);
        // build register info
        InstanceRegisterRequest registerRequest = new InstanceRegisterRequest();
        registerRequest.setNamespace(namespace);
        registerRequest.setService(registerDTO.getAppName());
        registerRequest.setHost(registerDTO.getHost());
        registerRequest.setPort(registerDTO.getPort());
        registerRequest.setProtocol(registerDTO.getProtocol());

        String metaDataJson = GsonUtils.getInstance().toJson(registerDTO);
        Map<String, String> metaDataMap = GsonUtils.getGson()
                .fromJson(metaDataJson, new TypeToken<Map<String, String>>() { }.getType());
        registerRequest.setMetadata(metaDataMap);

        // register service
        try {
            providerAPI.registerInstance(registerRequest);
        } catch (Exception e) {
            LOGGER.error("polaris client register uri failed: ", e);
            throw new ShenyuException(e);
        }
        LOGGER.info("polaris client register uri success: {}", registerDTO);
    }

    private void registerMetaData(final MetaDataRegisterDTO metadata) {
        String name = RegisterPathConstants.buildMetaDataParentPath(metadata.getRpcType(), metadata.getContextPath());
        final String namespace = props.getProperty(NAMESPACE, PolarisPathConstants.NAMESPACE);
        final String group = props.getProperty("fileGroup", PolarisPathConstants.FILE_GROUP);

        ConfigFileMetadata configFileMetadata = new DefaultConfigFileMetadata(namespace, group, name);
        metaDataRegisterDTOList.add(metadata);
        try {
            final String content = GsonUtils.getInstance().toJson(metaDataRegisterDTOList);
            final ConfigFile configFile = configFileService.getConfigFile(configFileMetadata);
            if (configFile.hasContent()) {
                configFilePublishService.updateConfigFile(configFileMetadata, content);
            } else {
                configFilePublishService.createConfigFile(configFileMetadata, content);
            }
        } catch (Exception e) {
            LOGGER.error("polaris client register meta-data failed: ", e);
            throw new ShenyuException(e);
        }
    }

    /**
     * close  providerAPI.
     */
    @Override
    public void closeRepository() {
        providerAPI.close();
    }
}
