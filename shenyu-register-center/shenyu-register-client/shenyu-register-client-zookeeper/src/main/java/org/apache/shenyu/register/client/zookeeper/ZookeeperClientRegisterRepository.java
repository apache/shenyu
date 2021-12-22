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

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import org.I0Itec.zkclient.IZkStateListener;
import org.I0Itec.zkclient.ZkClient;
import org.apache.shenyu.common.constant.Constants;
import static org.apache.shenyu.common.constant.Constants.PATH_SEPARATOR;
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
import org.apache.zookeeper.Watcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The type Zookeeper client register repository.
 */
@Join
public class ZookeeperClientRegisterRepository implements ShenyuClientRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ZookeeperClientRegisterRepository.class);

    private ZkClient zkClient;

    private final Map<String, String> nodeDataMap = new HashMap<>();

    public ZookeeperClientRegisterRepository() { }

    public ZookeeperClientRegisterRepository(final ShenyuRegisterCenterConfig config) {
        init(config);
    }

    @Override
    public void init(final ShenyuRegisterCenterConfig config) {
        Properties props = config.getProps();
        int sessionTimeout = Integer.parseInt(props.getProperty("sessionTimeout", "3000"));
        int connectionTimeout = Integer.parseInt(props.getProperty("connectionTimeout", "3000"));
        this.zkClient = new ZkClient(config.getServerLists(), sessionTimeout, connectionTimeout);
        this.zkClient.subscribeStateChanges(new ZkStateListener());
    }

    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        String rpcType = metadata.getRpcType();
        String contextPath = ContextPathUtils.buildRealNode(metadata.getContextPath(), metadata.getAppName());
        registerMetadata(rpcType, contextPath, metadata);
        LOGGER.info("{} zookeeper client register success: {}", rpcType, metadata);
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
    }
    
    @Override
    public void close() {
        zkClient.close();
    }

    private void registerMetadata(final String rpcType,
                                  final String contextPath,
                                  final MetaDataRegisterDTO metadata) {
        String metadataNodeName = buildMetadataNodeName(metadata);
        String metaDataPath = RegisterPathConstants.buildMetaDataParentPath(rpcType, contextPath);
        if (!zkClient.exists(metaDataPath)) {
            zkClient.createPersistent(metaDataPath, true);
        }
        String realNode = RegisterPathConstants.buildRealNode(metaDataPath, metadataNodeName);
        if (zkClient.exists(realNode)) {
            zkClient.writeData(realNode, GsonUtils.getInstance().toJson(metadata));
        } else {
            // if contextPath has two /, in zk means two folder, we need to create parent folder first, or else it will throw no node exception
            zkClient.createPersistent(realNode, true);
            zkClient.writeData(realNode, GsonUtils.getInstance().toJson(metadata));
        }
    }
    
    private synchronized void registerURI(final String rpcType, final String contextPath, final URIRegisterDTO registerDTO) {
        String uriNodeName = buildURINodeName(registerDTO);
        String uriPath = RegisterPathConstants.buildURIParentPath(rpcType, contextPath);
        if (!zkClient.exists(uriPath)) {
            zkClient.createPersistent(uriPath, true);
        }
        String realNode = RegisterPathConstants.buildRealNode(uriPath, uriNodeName);
        if (!zkClient.exists(realNode)) {
            String nodeData = GsonUtils.getInstance().toJson(registerDTO);
            nodeDataMap.put(realNode, nodeData);
            zkClient.createEphemeral(realNode, nodeData);
        }
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

    private class ZkStateListener implements IZkStateListener {
        @Override
        public void handleStateChanged(final Watcher.Event.KeeperState keeperState) {
            if (Watcher.Event.KeeperState.SyncConnected.equals(keeperState)) {
                nodeDataMap.forEach((k, v) -> {
                    if (!zkClient.exists(k)) {
                        zkClient.createEphemeral(k, v);
                        LOGGER.info("zookeeper client register success: {}", v);
                    }
                });
            }
        }

        @Override
        public void handleNewSession() {
        }

        @Override
        public void handleSessionEstablishmentError(final Throwable throwable) {
        }
    }
}
