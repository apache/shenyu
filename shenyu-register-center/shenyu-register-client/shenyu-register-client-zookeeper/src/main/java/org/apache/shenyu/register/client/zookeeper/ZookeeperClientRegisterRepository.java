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

import lombok.extern.slf4j.Slf4j;
import org.I0Itec.zkclient.IZkStateListener;
import org.I0Itec.zkclient.ZkClient;
import org.apache.zookeeper.Watcher;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.spi.Join;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * The type Zookeeper client register repository.
 */
@Join
@Slf4j
public class ZookeeperClientRegisterRepository implements ShenyuClientRegisterRepository {

    private ZkClient zkClient;

    private final Map<String, String> nodeDataMap = new HashMap<>();

    @Override
    public void init(final ShenyuRegisterCenterConfig config) {
        Properties props = config.getProps();
        int zookeeperSessionTimeout = Integer.parseInt(props.getProperty("zookeeperSessionTimeout", "3000"));
        int zookeeperConnectionTimeout = Integer.parseInt(props.getProperty("zookeeperConnectionTimeout", "3000"));
        this.zkClient = new ZkClient(config.getServerLists(), zookeeperSessionTimeout, zookeeperConnectionTimeout);
        this.zkClient.subscribeStateChanges(new ZkStateListener());
    }

    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        String rpcType = metadata.getRpcType();
        String contextPath = metadata.getContextPath().substring(1);
        registerMetadata(rpcType, contextPath, metadata);
        if (RpcTypeEnum.HTTP.getName().equals(rpcType) || RpcTypeEnum.TARS.getName().equals(rpcType) || RpcTypeEnum.GRPC.getName().equals(rpcType)) {
            registerURI(rpcType, contextPath, metadata);
        }
        log.info("{} zookeeper client register success: {}", rpcType, metadata);
    }

    @Override
    public void close() {
        zkClient.close();
    }

    private void registerMetadata(final String rpcType, final String contextPath, final MetaDataRegisterDTO metadata) {
        String metadataNodeName = buildMetadataNodeName(metadata);
        String metaDataPath = RegisterPathConstants.buildMetaDataParentPath(rpcType, contextPath);
        if (!zkClient.exists(metaDataPath)) {
            zkClient.createPersistent(metaDataPath, true);
        }
        String realNode = RegisterPathConstants.buildRealNode(metaDataPath, metadataNodeName);
        if (zkClient.exists(realNode)) {
            zkClient.writeData(realNode, GsonUtils.getInstance().toJson(metadata));
        } else {
            zkClient.createPersistent(realNode, GsonUtils.getInstance().toJson(metadata));
        }
    }

    private synchronized void registerURI(final String rpcType, final String contextPath, final MetaDataRegisterDTO metadata) {
        String uriNodeName = buildURINodeName(metadata);
        String uriPath = RegisterPathConstants.buildURIParentPath(rpcType, contextPath);
        if (!zkClient.exists(uriPath)) {
            zkClient.createPersistent(uriPath, true);
        }
        String realNode = RegisterPathConstants.buildRealNode(uriPath, uriNodeName);
        if (!zkClient.exists(realNode)) {
            String nodeData = GsonUtils.getInstance().toJson(URIRegisterDTO.transForm(metadata));
            nodeDataMap.put(realNode, nodeData);
            zkClient.createEphemeral(realNode, nodeData);
        }
    }

    private String buildURINodeName(final MetaDataRegisterDTO metadata) {
        String host = metadata.getHost();
        int port = metadata.getPort();
        return String.join(":", host, Integer.toString(port));
    }

    private String buildMetadataNodeName(final MetaDataRegisterDTO metadata) {
        String nodeName;
        String rpcType = metadata.getRpcType();
        if (RpcTypeEnum.HTTP.getName().equals(rpcType) || RpcTypeEnum.SPRING_CLOUD.getName().equals(rpcType)) {
            nodeName = String.join("-", metadata.getContextPath(), metadata.getRuleName().replace("/", "-"));
        } else {
            nodeName = RegisterPathConstants.buildNodeName(metadata.getServiceName(), metadata.getMethodName());
        }
        return nodeName.startsWith("/") ? nodeName.substring(1) : nodeName;
    }

    private class ZkStateListener implements IZkStateListener {
        @Override
        public void handleStateChanged(final Watcher.Event.KeeperState keeperState) {
            if (Watcher.Event.KeeperState.SyncConnected.equals(keeperState)) {
                nodeDataMap.forEach((k, v) -> {
                    if (!zkClient.exists(k)) {
                        zkClient.createEphemeral(k, v);
                        log.info("zookeeper client register success: {}", v);
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
