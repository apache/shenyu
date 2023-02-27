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

package org.apache.shenyu.register.client.server.polaris;

import com.tencent.polaris.api.config.Configuration;
import com.tencent.polaris.api.core.ConsumerAPI;
import com.tencent.polaris.api.listener.ServiceListener;
import com.tencent.polaris.api.pojo.Instance;
import com.tencent.polaris.api.pojo.ServiceChangeEvent;
import com.tencent.polaris.api.rpc.WatchServiceRequest;
import com.tencent.polaris.client.api.SDKContext;
import com.tencent.polaris.configuration.api.core.ConfigFile;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.api.core.ConfigKVFile;
import com.tencent.polaris.configuration.factory.ConfigFileServiceFactory;
import com.tencent.polaris.factory.ConfigAPIFactory;
import com.tencent.polaris.factory.api.DiscoveryAPIFactory;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Polaris register server.
 */
@Join
public class PolarisClientServerRegisterRepository implements ShenyuClientServerRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(PolarisClientServerRegisterRepository.class);

    private ConsumerAPI consumerAPI;

    private ConfigFileService configFileService;

    private ShenyuClientServerRegisterPublisher publisher;

    @Override
    public void init(final ShenyuClientServerRegisterPublisher publisher, final ShenyuRegisterCenterConfig config) {
        Configuration configuration = buildConfiguration(config);
        SDKContext sdkContext = SDKContext.initContextByConfig(configuration);
        ConsumerAPI consumerAPI = DiscoveryAPIFactory.createConsumerAPIByContext(sdkContext);

        this.consumerAPI = consumerAPI;
        this.publisher = publisher;
        this.configFileService = ConfigFileServiceFactory.createConfigFileService(sdkContext);
        subscribe();
    }

    private Configuration buildConfiguration(final ShenyuRegisterCenterConfig config) {
        String serverLists = config.getServerLists();
        Configuration configuration = ConfigAPIFactory.createConfigurationByAddress(serverLists);
        return configuration;
    }

    private void subscribe() {
        WatchServiceRequest watchServiceRequest = WatchServiceRequest.builder()
                .namespace("default")
                .service("shenyu-client-demo")
                .listeners(Arrays.asList(new ServiceListener() {
                    @Override
                    public void onEvent(ServiceChangeEvent event) {
                        LOGGER.info("t");
                        List<Instance> allInstances = event.getAllInstances();
                        for (Instance instance : allInstances) {
                            if (instance.isHealthy()) {
                                Map<String, String> metaMap = instance.getMetadata();
                                URIRegisterDTO uriRegisterDTO = GsonUtils.getInstance().fromJson(GsonUtils.getInstance().toJson(metaMap),URIRegisterDTO.class);
                                uriRegisterDTO.setPort(instance.getPort());
                                uriRegisterDTO.setEventType(EventType.REGISTER);
                                publisher.publish(uriRegisterDTO);

                                String fileName = RegisterPathConstants.buildServiceConfigPath(uriRegisterDTO.getRpcType(),uriRegisterDTO.getContextPath());
                                ConfigFile configFile = configFileService.getConfigFile("default", "shenyu", fileName);
                                List<MetaDataRegisterDTO> registerMetadataList = GsonUtils.getInstance().fromList(configFile.getContent(), MetaDataRegisterDTO.class);
                                for (MetaDataRegisterDTO metaDataDto : registerMetadataList) {
                                    publisher.publish(metaDataDto);
                                }
                            }
                        }
                    }
                })).build();
        consumerAPI.watchService(watchServiceRequest);
    }

    @Override
    public void close() {
        this.consumerAPI.close();
    }
}
