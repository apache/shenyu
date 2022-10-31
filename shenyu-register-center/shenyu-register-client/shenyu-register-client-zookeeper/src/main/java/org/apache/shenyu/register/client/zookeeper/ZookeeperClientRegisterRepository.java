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

package org.apache.shenyu.register.client.zookeeper;

import org.apache.commons.lang3.StringUtils;
import org.apache.curator.framework.state.ConnectionState;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.ContextPathUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.spi.Join;
import org.apache.zookeeper.CreateMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import static org.apache.shenyu.common.constant.Constants.PATH_SEPARATOR;

/**
 * The type Zookeeper client register repository.
 */
@Join
public class ZookeeperClientRegisterRepository implements ShenyuClientRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ZookeeperClientRegisterRepository.class);

    private ZookeeperClient client;

    private final Map<String, String> nodeDataMap = new HashMap<>();

    private final Set<String> metadataSet = new HashSet<>();

    public ZookeeperClientRegisterRepository() {
    }

    public ZookeeperClientRegisterRepository(final ShenyuRegisterCenterConfig config) {
        init(config);
    }

    @Override
    public void init(final ShenyuRegisterCenterConfig config) {
        Properties props = config.getProps();
        int sessionTimeout = Integer.parseInt(props.getProperty("sessionTimeout", "3000"));
        int connectionTimeout = Integer.parseInt(props.getProperty("connectionTimeout", "3000"));

        int baseSleepTime = Integer.parseInt(props.getProperty("baseSleepTime", "1000"));
        int maxRetries = Integer.parseInt(props.getProperty("maxRetries", "3"));
        int maxSleepTime = Integer.parseInt(props.getProperty("maxSleepTime", String.valueOf(Integer.MAX_VALUE)));

        ZookeeperConfig zkConfig = new ZookeeperConfig(config.getServerLists());
        zkConfig.setBaseSleepTimeMilliseconds(baseSleepTime)
                .setMaxRetries(maxRetries)
                .setMaxSleepTimeMilliseconds(maxSleepTime)
                .setSessionTimeoutMilliseconds(sessionTimeout)
                .setConnectionTimeoutMilliseconds(connectionTimeout);

        String digest = props.getProperty("digest");
        if (!StringUtils.isEmpty(digest)) {
            zkConfig.setDigest(digest);
        }

        this.client = new ZookeeperClient(zkConfig);
        this.client.getClient().getConnectionStateListenable().addListener((c, newState) -> {
            if (newState == ConnectionState.RECONNECTED) {
                nodeDataMap.forEach((k, v) -> {
                    if (!client.isExist(k)) {
                        client.createOrUpdate(k, v, CreateMode.EPHEMERAL);
                        LOGGER.info("zookeeper client register uri success: {}", v);
                    }
                });
            }
        });

        client.start();
    }

    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        String rpcType = metadata.getRpcType();
        String contextPath = ContextPathUtils.buildRealNode(metadata.getContextPath(), metadata.getAppName());
        registerMetadata(rpcType, contextPath, metadata);
    }

    /**
     * Persist uri.
     *
     * @param registerDTO the register dto
     */
    @Override
    public void persistURI(final URIRegisterDTO registerDTO) {
        String rpcType = registerDTO.getRpcType();
        String contextPath = ContextPathUtils.buildRealNode(registerDTO.getContextPath(), registerDTO.getAppName());
        registerURI(rpcType, contextPath, registerDTO);
        LOGGER.info("{} zookeeper client register uri success: {}", rpcType, registerDTO);
    }

    @Override
    public void close() {
        this.client.close();
    }

    private void registerMetadata(final String rpcType,
                                  final String contextPath,
                                  final MetaDataRegisterDTO metadata) {
        String metadataNodeName = buildMetadataNodeName(metadata);
        String metaDataPath = RegisterPathConstants.buildMetaDataParentPath(rpcType, contextPath);
        String realNode = RegisterPathConstants.buildRealNode(metaDataPath, metadataNodeName);
        // avoid dup registration for metadata
        synchronized (metadataSet) {
            if (metadataSet.contains(realNode)) {
                return;
            }
            metadataSet.add(realNode);
        }
        client.createOrUpdate(realNode, metadata, CreateMode.PERSISTENT);
        LOGGER.info("{} zookeeper client register metadata success: {}", rpcType, metadata);
    }

    private synchronized void registerURI(final String rpcType, final String contextPath, final URIRegisterDTO registerDTO) {
        String uriNodeName = buildURINodeName(registerDTO);
        String uriPath = RegisterPathConstants.buildURIParentPath(rpcType, contextPath);
        String realNode = RegisterPathConstants.buildRealNode(uriPath, uriNodeName);
        String nodeData = GsonUtils.getInstance().toJson(registerDTO);
        nodeDataMap.put(realNode, nodeData);
        client.createOrUpdate(realNode, nodeData, CreateMode.EPHEMERAL);
    }

    private String buildURINodeName(final URIRegisterDTO registerDTO) {
        String host = registerDTO.getHost();
        int port = registerDTO.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    private String buildMetadataNodeName(final MetaDataRegisterDTO metadata) {
        String nodeName;
        String rpcType = metadata.getRpcType();
        if (RpcTypeEnum.HTTP.getName().equals(rpcType)
                || RpcTypeEnum.SPRING_CLOUD.getName().equals(rpcType)) {
            nodeName = String.join(DefaultPathConstants.SELECTOR_JOIN_RULE,
                    metadata.getContextPath(),
                    metadata.getRuleName().replace(PATH_SEPARATOR, DefaultPathConstants.SELECTOR_JOIN_RULE));
        } else {
            nodeName = RegisterPathConstants.buildNodeName(metadata.getServiceName(), metadata.getMethodName());
        }
        return nodeName.startsWith(PATH_SEPARATOR) ? nodeName.substring(1) : nodeName;
    }
}
