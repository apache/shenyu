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

package org.dromara.soul.register.server.zookeeper;

import lombok.extern.slf4j.Slf4j;
import org.I0Itec.zkclient.IZkDataListener;
import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.register.common.config.SoulRegisterCenterConfig;
import org.dromara.soul.register.common.path.ZkRegisterPathConstants;
import org.dromara.soul.register.server.api.SoulServerRegisterRepository;
import org.dromara.soul.register.server.api.SoulSeverRegisterCenterEventPublisher;
import org.dromara.soul.register.server.api.listener.DataChangedEvent;
import org.dromara.soul.register.server.api.listener.DataChangedEventListener;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

/**
 * Zookeeper register center.
 *
 * @author lw1243925457
 */
@Slf4j
public class ZookeeperServerRegisterRepository implements SoulServerRegisterRepository {

    private final SoulSeverRegisterCenterEventPublisher publisher;
    
    private ZkClient zkClient;

    private final String rootPath = ZkRegisterPathConstants.ROOT_PATH;

    private List<String> rpcTypePathList = new ArrayList<>();

    private List<String> contextPathList = new ArrayList<>();
    
    public ZookeeperServerRegisterRepository(final SoulSeverRegisterCenterEventPublisher publisher,
                                             final SoulRegisterCenterConfig soulRegisterCenterConfig) {
        this.publisher = publisher;
        init(soulRegisterCenterConfig);
    }
    
    @Override
    public void init(final SoulRegisterCenterConfig config) {
        Properties props = config.getProps();
        int zookeeperSessionTimeout = Integer.parseInt(props.getProperty("zookeeperSessionTimeout", "3000"));
        int zookeeperConnectionTimeout = Integer.parseInt(props.getProperty("zookeeperConnectionTimeout", "3000"));
        this.zkClient = new ZkClient(config.getServerLists(), zookeeperSessionTimeout, zookeeperConnectionTimeout);
        initSubscribe();
    }

    @Override
    public void close() {
        zkClient.close();
    }
    
    @Override
    public void watch(final String key, final DataChangedEventListener listener) {
    
    }

    /**
     * rpc类型、context path、都会变化增加，监听其变化.
     */
    private void initSubscribe() {
        log.info("zookeeper register watch and data init start");
        subscribeRoot()
                .forEach(rpcTypePath -> subscribeRpcType(rpcTypePath)
                        .forEach(this::subscribeContext));
    }

    private List<String> subscribeRoot() {
        log.info("watch root node");

        zkClient.subscribeChildChanges(rootPath, (parent, currentChildren) -> {
            log.info("rpc type node change: {}", currentChildren);
            if (!currentChildren.isEmpty()) {
                currentChildren.forEach(rpcType -> {
                    if (!rpcTypePathList.contains(rpcType)) {
                        subscribeRpcType(buildPath(parent, rpcType)).forEach(this::subscribeContext);
                        rpcTypePathList.add(rpcType);
                    }
                });
            }
        });

        rpcTypePathList = zkClientGetChildren(rootPath);
        List<String> rpcTypePaths = new ArrayList<>();
        rpcTypePathList.forEach(rpcType -> {
            rpcTypePaths.add(buildPath(rootPath, rpcType));
        });
        return rpcTypePaths;
    }

    private String buildPath(final String prefix, final String node) {
        return String.join(Constants.SLASH, prefix, node);
    }

    private List<String> subscribeRpcType(final String rpcTypePath) {
        log.info("watch rpc type child: {}", rpcTypePath);

        zkClient.subscribeChildChanges(rpcTypePath, (parent, currentChildren) -> {
            log.info("context node change: {}", currentChildren);
            if (!currentChildren.isEmpty()) {
                currentChildren.forEach(context -> {
                    if (!contextPathList.contains(context)) {
                        contextPathList.add(context);
                        subscribeContext(buildPath(rpcTypePath, context));
                    }
                });
            }
        });

        List<String> contextPaths = new ArrayList<>();
        List<String> contexts = zkClientGetChildren(rpcTypePath);
        contexts.forEach(context -> {
            if (!contextPathList.contains(context)) {
                contextPathList.add(context);
            }
            contextPaths.add(buildPath(rpcTypePath, context));
        });
        return contextPaths;
    }

    private void subscribeContext(final String contextPath) {
        log.info("watch context node and init service : {}", contextPath);

        updateMetadata(contextPath);

        zkClient.subscribeChildChanges(contextPath, (parent, currentChildren) -> {
            log.info("context node change: {}", currentChildren);
            updateMetadata(contextPath);
        });
    }

    private void updateMetadata(final String contextPath) {
        List<String> uriList = new ArrayList<>();

        zkClientGetChildren(contextPath).forEach(metadata -> {
            String metadataPath = buildPath(contextPath, metadata);
            String uri = zkClient.readData(metadataPath);
            if (uri == null) {
                subscribeMetadata(metadataPath);
                updateService(metadataPath);
            } else {
                uriList.add(uri);
                subscribeUriChange(contextPath, uri);
            }
        });

        List<String> paths = Arrays.asList(contextPath.split(Constants.SLASH));
        String context = paths.get(paths.size() - 1);
        updateSelectorHandler("/" + context, uriList);
    }

    private void subscribeMetadata(final String metadataPath) {
        zkClient.subscribeChildChanges(metadataPath, (parent, currentChildren) -> {
            currentChildren.forEach(service -> {
                String servicePath = buildPath(metadataPath, service);
                String type = getType(servicePath);
                String data = zkClient.readData(servicePath);
                if (data != null) {
                    register(type, data);
                }
            });
        });
    }

    private void subscribeUriChange(final String contextPath, final String uri) {
        String uriNode = String.join(Constants.SLASH, contextPath, uri);
        log.info("subscribe uri data: {}", uriNode);
        zkClient.subscribeDataChanges(uriNode, new IZkDataListener() {

            @Override
            public void handleDataChange(final String s, final Object o) {
                log.info("uri data change: {} {}", s, o);
            }

            @Override
            public void handleDataDeleted(final String path) {
                log.info("uri delete: {}", path);
                zkClient.unsubscribeDataChanges(path, this);
                updateContextNode(contextPath);
            }
        });
    }

    private void updateService(final String metadataPath) {
        zkClientGetChildren(metadataPath).forEach(service -> {
            String servicePath = buildPath(metadataPath, service);
            String type = getType(servicePath);
            String data = zkClient.readData(servicePath);
            if (data != null) {
                register(type, data);
            }
        });
    }

    /**
     * get rpc type.
     *
     * @param dataPath path
     * @return rpc type
     */
    private String getType(final String dataPath) {
        return dataPath.split(Constants.SLASH)[3];
    }

    /**
     * register in center.
     *
     * @param type rpc type
     * @param data dto data
     */
    private void register(final String type, final String data) {
        publisher.publishEvent(DataChangedEvent.Type.REGISTER, type, data);
    }

    /**
     * get zk nodes of path.
     *
     * @param parent parent path
     * @return nodes
     */
    private List<String> zkClientGetChildren(final String parent) {
        if (!zkClient.exists(parent)) {
            zkClient.createPersistent(parent, true);
        }
        return zkClient.getChildren(parent);
    }

    /**
     * update divide upstream.
     *
     * @param dataPath data path
     */
    private void updateContextNode(final String dataPath) {
        List<String> path = Arrays.asList(dataPath.split("/"));
        String rpcType = path.get(3);
        String context = path.get(4);
        if (!RpcTypeEnum.HTTP.getName().equals(rpcType)) {
            return;
        }
        List<String> uriList = getNodeData(rpcType, context);
        updateSelectorHandler("/" + context, uriList);
    }

    /**
     * get uri list of context.
     *
     * @param type rpc type
     * @param context context
     * @return uri list
     */
    private List<String> getNodeData(final String type, final String context) {
        List<String> uriList = new ArrayList<>();
        String parentPath = String.join(Constants.SLASH, rootPath, type, context);
        zkClientGetChildren(parentPath).forEach(nodePath -> {
            String uri = zkClient.readData(String.join(Constants.SLASH, parentPath, nodePath));
            if (uri != null) {
                uriList.add(uri);
            }
        });
        return uriList;
    }

    /**
     * update selector and publisher event.
     *
     * @param contextPath context
     * @param uriList uri list
     */
    private void updateSelectorHandler(final String contextPath, final List<String> uriList) {
        log.info("update selector: {} -- {}", contextPath, uriList);
        publisher.publishEvent(DataChangedEvent.Type.UPDATED, contextPath, uriList);
    }
}
