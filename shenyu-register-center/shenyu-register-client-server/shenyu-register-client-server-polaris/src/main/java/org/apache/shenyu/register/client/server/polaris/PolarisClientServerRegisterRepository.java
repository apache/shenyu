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
import com.tencent.polaris.api.pojo.Instance;
import com.tencent.polaris.api.rpc.WatchServiceRequest;
import com.tencent.polaris.client.api.SDKContext;
import com.tencent.polaris.configuration.api.core.ConfigFile;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.factory.ConfigFileServiceFactory;
import com.tencent.polaris.factory.ConfigAPIFactory;
import com.tencent.polaris.factory.api.DiscoveryAPIFactory;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import static org.apache.shenyu.common.constant.Constants.NAMESPACE;
import org.apache.shenyu.common.constant.PolarisPathConstants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.spi.Join;

/**
 * Polaris register server.
 */
@Join
public class PolarisClientServerRegisterRepository implements ShenyuClientServerRegisterRepository {

    private ConsumerAPI consumerAPI;

    private ConfigFileService configFileService;

    private ShenyuClientServerRegisterPublisher publisher;

    private Properties props;

    @Override
    public void init(final ShenyuClientServerRegisterPublisher publisher, final ShenyuRegisterCenterConfig config) {
        Configuration configuration = buildConfiguration(config);
        SDKContext sdkContext = SDKContext.initContextByConfig(configuration);

        this.consumerAPI = DiscoveryAPIFactory.createConsumerAPIByContext(sdkContext);
        this.publisher = publisher;
        this.configFileService = ConfigFileServiceFactory.createConfigFileService(sdkContext);
        this.props = config.getProps();
        subscribe();
    }

    private Configuration buildConfiguration(final ShenyuRegisterCenterConfig config) {
        String serverLists = config.getServerLists();
        return ConfigAPIFactory.createConfigurationByAddress(serverLists);
    }

    private void subscribe() {
        RpcTypeEnum.acquireSupportMetadatas().forEach(this::subscribe);
    }

    private void subscribe(final RpcTypeEnum rpcTypeEnum) {
        final String namespace = props.getProperty(NAMESPACE, PolarisPathConstants.NAMESPACE);
        final String serviceName = RegisterPathConstants.buildServiceInstancePath(rpcTypeEnum.getName());
        final String group = props.getProperty("fileGroup", PolarisPathConstants.FILE_GROUP);
        WatchServiceRequest watchServiceRequest = WatchServiceRequest.builder()
                .namespace(namespace)
                .service(serviceName)
                .listeners(Collections.singletonList(event -> {
                    List<Instance> allInstances = event.getAllInstances();
                    for (Instance instance : allInstances) {
                        if (!instance.isHealthy()) {
                            continue;
                        }
                        Map<String, String> metaMap = instance.getMetadata();
                        URIRegisterDTO uriRegisterDTO = GsonUtils.getInstance().fromJson(GsonUtils.getInstance().toJson(metaMap), URIRegisterDTO.class);
                        uriRegisterDTO.setPort(instance.getPort());
                        uriRegisterDTO.setEventType(EventType.REGISTER);
                        publisher.publish(uriRegisterDTO);

                        String fileName = RegisterPathConstants.buildServiceConfigPath(uriRegisterDTO.getRpcType(), uriRegisterDTO.getContextPath());
                        ConfigFile configFile = configFileService.getConfigFile(namespace, group, fileName);
                        List<MetaDataRegisterDTO> registerMetadataList = GsonUtils.getInstance().fromList(configFile.getContent(), MetaDataRegisterDTO.class);
                        for (MetaDataRegisterDTO metaDataDto : registerMetadataList) {
                            publisher.publish(metaDataDto);
                        }
                    }
                })).build();
        consumerAPI.watchService(watchServiceRequest);
    }

    @Override
    public void close() {
        this.consumerAPI.close();
        this.publisher.close();
    }
}
