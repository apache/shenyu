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

package org.dromara.soul.register.server.nacos;

import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.api.config.ConfigFactory;
import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.config.listener.Listener;
import com.alibaba.nacos.api.naming.NamingFactory;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.naming.listener.NamingEvent;
import com.alibaba.nacos.api.naming.pojo.Instance;
import com.google.common.collect.Lists;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.register.common.config.SoulRegisterCenterConfig;
import org.dromara.soul.register.common.dto.MetaDataRegisterDTO;
import org.dromara.soul.register.common.dto.URIRegisterDTO;
import org.dromara.soul.register.common.path.RegisterPathConstants;
import org.dromara.soul.register.server.api.SoulServerRegisterPublisher;
import org.dromara.soul.register.server.api.SoulServerRegisterRepository;
import org.dromara.soul.spi.Join;

import java.util.EnumSet;
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
 * nacos register server.
 *
 * @author lw1243925457
 */
@Slf4j
@Join
public class NacosServerRegisterRepository implements SoulServerRegisterRepository {

    private static final EnumSet<RpcTypeEnum> RPC_TYPE_SET = EnumSet.of(RpcTypeEnum.DUBBO, RpcTypeEnum.GRPC,
            RpcTypeEnum.HTTP, RpcTypeEnum.SPRING_CLOUD, RpcTypeEnum.SOFA, RpcTypeEnum.TARS);

    private String defaultGroup = "default_group";

    private ConfigService configService;

    private NamingService namingService;

    private SoulServerRegisterPublisher publisher;

    private final ConcurrentSkipListSet<String> metadataConfigCache = new ConcurrentSkipListSet<>();

    private final ConcurrentMap<String, ConcurrentSkipListSet<String>> uriServiceCache = new ConcurrentHashMap<>();

    @Override
    public void close() {
        publisher.close();
    }

    @SneakyThrows
    @Override
    public void init(final SoulServerRegisterPublisher publisher, final SoulRegisterCenterConfig config) {
        this.publisher = publisher;
        String serverAddr = config.getServerLists();
        Properties properties = config.getProps();
        Properties nacosProperties = new Properties();
        nacosProperties.put(PropertyKeyConst.SERVER_ADDR, serverAddr);
        String nameSpace = "nacosNameSpace";
        nacosProperties.put(PropertyKeyConst.NAMESPACE, properties.getProperty(nameSpace));
        this.configService = ConfigFactory.createConfigService(nacosProperties);
        this.namingService = NamingFactory.createNamingService(nacosProperties);
        subscribe();
    }

    private void subscribe() {
        RPC_TYPE_SET.forEach(this::subscribeRpcTypeService);
    }

    @SneakyThrows
    private void subscribeRpcTypeService(final RpcTypeEnum rpcType) {
        final String serviceName = RegisterPathConstants.buildServiceInstancePath(rpcType.getName());

        Map<String, List<URIRegisterDTO>> services = new HashMap<>();
        List<Instance> healthyInstances = namingService.selectInstances(serviceName, true);
        healthyInstances.forEach(healthyInstance -> {
            String contextPath = healthyInstance.getMetadata().get("contextPath");
            String serviceConfigName = RegisterPathConstants.buildServiceConfigPath(rpcType.getName(), contextPath);
            subscribeMetadata(serviceConfigName);
            metadataConfigCache.add(serviceConfigName);
            String metadata = healthyInstance.getMetadata().get("uriMetadata");
            URIRegisterDTO uriRegisterDTO = GsonUtils.getInstance().fromJson(metadata, URIRegisterDTO.class);
            services.computeIfAbsent(contextPath, k -> new ArrayList<>()).add(uriRegisterDTO);
            uriServiceCache.computeIfAbsent(serviceName, k -> new ConcurrentSkipListSet<>()).add(contextPath);
        });

        for (List<URIRegisterDTO> uriRegisterDTOList: services.values()) {
            if (rpcType.equals(RpcTypeEnum.HTTP) || rpcType.equals(RpcTypeEnum.TARS) || rpcType.equals(RpcTypeEnum.GRPC)) {
                publishRegisterURI(uriRegisterDTOList);
            }
        }
        log.info("subscribe uri : {}", serviceName);
        namingService.subscribe(serviceName, event -> {
            if (event instanceof NamingEvent) {
                List<Instance> instances = ((NamingEvent) event).getInstances();
                instances.forEach(instance -> {
                    String contextPath = instance.getMetadata().get("contextPath");
                    uriServiceCache.computeIfAbsent(serviceName, k -> new ConcurrentSkipListSet<>()).add(contextPath);
                });
                refreshURIService(rpcType.getName(), serviceName);
            }
        });
    }

    @SneakyThrows
    private void subscribeMetadata(final String serviceConfigName) {
        registerMetadata(readData(serviceConfigName));

        log.info("subscribe metadata: {}", serviceConfigName);
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
    }

    private void registerMetadata(final String metadataConfig) {
        List<String> metadataList = GsonUtils.getInstance().fromJson(metadataConfig, List.class);
        metadataList.forEach(this::publishMetadata);
    }

    private void publishMetadata(final String data) {
        log.info("publish metadata: {}", data);
        publisher.publish(Lists.newArrayList(GsonUtils.getInstance().fromJson(data, MetaDataRegisterDTO.class)));
    }

    private void refreshURIService(final String rpcType, final String serviceName) {
        for (String contextPath: uriServiceCache.get(serviceName)) {
            registerURI(contextPath, serviceName, rpcType);
        }
    }

    @SneakyThrows
    private void registerURI(final String contextPath, final String serviceName, final String rpcType) {
        List<Instance> healthyInstances = namingService.selectInstances(serviceName, true);

        List<URIRegisterDTO> registerDTOList = new ArrayList<>();
        healthyInstances.forEach(healthyInstance -> {
            if (contextPath.equals(healthyInstance.getMetadata().get("contextPath"))) {
                String metadata = healthyInstance.getMetadata().get("uriMetadata");
                URIRegisterDTO uriRegisterDTO = GsonUtils.getInstance().fromJson(metadata, URIRegisterDTO.class);
                registerDTOList.add(uriRegisterDTO);

                String serviceConfigName = RegisterPathConstants.buildServiceConfigPath(rpcType, contextPath);
                if (!metadataConfigCache.contains(serviceConfigName)) {
                    subscribeMetadata(serviceConfigName);
                    metadataConfigCache.add(serviceConfigName);
                }
            }
        });
        if (!RpcTypeEnum.acquireSupportURIs().contains(RpcTypeEnum.acquireByName(rpcType))) {
            return;
        }
        if (registerDTOList.isEmpty()) {
            URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder().contextPath("/" + contextPath).build();
            registerDTOList.add(uriRegisterDTO);
        }
        publishRegisterURI(registerDTOList);
    }

    private void publishRegisterURI(final List<URIRegisterDTO> registerDTOList) {
        log.info("publish uri: {}", registerDTOList);
        publisher.publish(registerDTOList);
    }

    @SneakyThrows
    private String readData(final String configName) {
        return configService.getConfig(configName, defaultGroup, 5000);
    }
}
