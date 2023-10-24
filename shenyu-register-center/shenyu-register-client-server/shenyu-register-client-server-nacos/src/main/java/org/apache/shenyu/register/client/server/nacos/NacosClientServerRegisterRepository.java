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

package org.apache.shenyu.register.client.server.nacos;

import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.api.config.ConfigFactory;
import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.config.listener.Listener;
import com.alibaba.nacos.api.exception.NacosException;
import com.alibaba.nacos.api.naming.NamingFactory;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.naming.listener.NamingEvent;
import com.alibaba.nacos.api.naming.pojo.Instance;
import com.alibaba.nacos.common.utils.StringUtils;
import com.google.common.collect.Lists;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.MapUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterRepository;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Optional;
import java.util.Properties;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;

/**
 * Nacos register server.
 */
@Join
public class NacosClientServerRegisterRepository implements ShenyuClientServerRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(NacosClientServerRegisterRepository.class);

    private static final List<RpcTypeEnum> RPC_URI_TYPE_SET = RpcTypeEnum.acquireSupportURIs();

    private final String defaultGroup = NacosPathConstants.GROUP;

    private ConfigService configService;

    private NamingService namingService;

    private ShenyuClientServerRegisterPublisher publisher;

    private final ConcurrentSkipListSet<String> metadataConfigCache = new ConcurrentSkipListSet<>();

    private final ConcurrentMap<String, ConcurrentSkipListSet<String>> uriServiceCache = new ConcurrentHashMap<>();

    @Override
    public void close() {
        publisher.close();
    }

    @Override
    public void init(final ShenyuClientServerRegisterPublisher publisher,
                     final ShenyuRegisterCenterConfig config) {
        this.publisher = publisher;
        String serverAddr = config.getServerLists();
        Properties properties = config.getProps();
        Properties nacosProperties = new Properties();
        nacosProperties.put(PropertyKeyConst.SERVER_ADDR, serverAddr);
        nacosProperties.put(PropertyKeyConst.NAMESPACE, properties.getProperty("nacosNameSpace"));
        // the nacos authentication username
        nacosProperties.put(PropertyKeyConst.USERNAME, properties.getProperty(PropertyKeyConst.USERNAME, ""));
        // the nacos authentication password
        nacosProperties.put(PropertyKeyConst.PASSWORD, properties.getProperty(PropertyKeyConst.PASSWORD, ""));
        // access key for namespace
        nacosProperties.put(PropertyKeyConst.ACCESS_KEY, properties.getProperty(PropertyKeyConst.ACCESS_KEY, ""));
        // secret key for namespace
        nacosProperties.put(PropertyKeyConst.SECRET_KEY, properties.getProperty(PropertyKeyConst.SECRET_KEY, ""));

        try {
            this.configService = ConfigFactory.createConfigService(nacosProperties);
            this.namingService = NamingFactory.createNamingService(nacosProperties);
        } catch (NacosException e) {
            throw new ShenyuException(e);
        }

        subscribe();
    }

    private void subscribe() {
        RpcTypeEnum.acquireSupportMetadatas().forEach(this::subscribeRpcTypeService);
    }

    private void subscribeRpcTypeService(final RpcTypeEnum rpcType) {
        final String serviceName = RegisterPathConstants.buildServiceInstancePath(rpcType.getName());
        try {
            Map<String, List<URIRegisterDTO>> services = new HashMap<>();
            List<Instance> healthyInstances = namingService.selectInstances(serviceName, true);
            healthyInstances.forEach(healthyInstance -> {
                String contextPath = healthyInstance.getMetadata().get("contextPath");
                String serviceConfigName = RegisterPathConstants.buildServiceConfigPath(rpcType.getName(), contextPath);
                subscribeMetadata(serviceConfigName);
                metadataConfigCache.add(serviceConfigName);
                String metadata = healthyInstance.getMetadata().get("uriMetadata");
                URIRegisterDTO uriRegisterDTO = GsonUtils.getInstance().fromJson(metadata, URIRegisterDTO.class);
                MapUtils.computeIfAbsent(services, contextPath, k -> new ArrayList<>()).add(uriRegisterDTO);
                MapUtils.computeIfAbsent(uriServiceCache, serviceName, k -> new ConcurrentSkipListSet<>()).add(contextPath);
            });
            if (RPC_URI_TYPE_SET.contains(rpcType)) {
                services.values().forEach(this::publishRegisterURI);
            }
            LOGGER.info("subscribe uri : {}", serviceName);
            namingService.subscribe(serviceName, event -> {
                if (event instanceof NamingEvent) {
                    List<Instance> instances = ((NamingEvent) event).getInstances();
                    instances.forEach(instance -> {
                        String contextPath = instance.getMetadata().get("contextPath");
                        MapUtils.computeIfAbsent(uriServiceCache, serviceName, k -> new ConcurrentSkipListSet<>()).add(contextPath);
                    });
                    refreshURIService(rpcType, serviceName);
                }
            });
        } catch (NacosException e) {
            throw new ShenyuException(e);
        }
    }

    private void subscribeMetadata(final String serviceConfigName) {
        String content = readData(serviceConfigName);
        if (StringUtils.isEmpty(content)) {
            return;
        }
        registerMetadata(content);
        LOGGER.info("subscribe metadata: {}", serviceConfigName);
        try {
            configService.addListener(serviceConfigName, defaultGroup, new Listener() {

                @Override
                public Executor getExecutor() {
                    return null;
                }

                @Override
                public void receiveConfigInfo(final String config) {
                    registerMetadata(config);
                }
            });
        } catch (NacosException e) {
            throw new ShenyuException(e);
        }
    }

    @SuppressWarnings("unchecked")
    private void registerMetadata(final String metadataConfig) {
        List<String> metadataList = GsonUtils.getInstance().fromJson(metadataConfig, List.class);
        metadataList.forEach(this::publishMetadata);
    }

    private void publishMetadata(final String data) {
        LOGGER.info("publish metadata: {}", data);
        publisher.publish(Lists.newArrayList(GsonUtils.getInstance().fromJson(data, MetaDataRegisterDTO.class)));
    }

    private void refreshURIService(final RpcTypeEnum rpcType, final String serviceName) {
        Optional.ofNullable(uriServiceCache.get(serviceName)).ifPresent(services -> services.forEach(contextPath -> registerURI(contextPath, serviceName, rpcType)));
    }

    private void registerURI(final String contextPath, final String serviceName, final RpcTypeEnum rpcType) {
        try {
            List<Instance> healthyInstances = namingService.selectInstances(serviceName, true);
            List<URIRegisterDTO> registerDTOList = new ArrayList<>();
            healthyInstances.forEach(healthyInstance -> {
                if (contextPath.equals(healthyInstance.getMetadata().get("contextPath"))) {
                    String metadata = healthyInstance.getMetadata().get("uriMetadata");
                    URIRegisterDTO uriRegisterDTO = GsonUtils.getInstance().fromJson(metadata, URIRegisterDTO.class);
                    registerDTOList.add(uriRegisterDTO);

                    String serviceConfigName = RegisterPathConstants.buildServiceConfigPath(rpcType.getName(), contextPath);
                    if (!metadataConfigCache.contains(serviceConfigName)) {
                        subscribeMetadata(serviceConfigName);
                        metadataConfigCache.add(serviceConfigName);
                    }
                }
            });
            if (!RPC_URI_TYPE_SET.contains(rpcType)) {
                return;
            }
            if (registerDTOList.isEmpty()) {
                URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder()
                        .contextPath(Constants.PATH_SEPARATOR + contextPath)
                        .rpcType(rpcType.getName()).build();
                registerDTOList.add(uriRegisterDTO);
            }
            publishRegisterURI(registerDTOList);
        } catch (NacosException e) {
            throw new ShenyuException(e);
        }
    }

    private void publishRegisterURI(final List<URIRegisterDTO> registerDTOList) {
        LOGGER.info("publish uri: {}", registerDTOList);
        publisher.publish(registerDTOList);
    }

    private String readData(final String configName) {
        try {
            return configService.getConfig(configName, defaultGroup, 5000);
        } catch (NacosException e) {
            throw new ShenyuException(e);
        }
    }
}
