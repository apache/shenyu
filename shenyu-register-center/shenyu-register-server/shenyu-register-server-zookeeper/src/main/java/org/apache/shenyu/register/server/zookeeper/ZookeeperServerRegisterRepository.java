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

package org.apache.shenyu.register.server.zookeeper;

import com.google.common.collect.Lists;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.I0Itec.zkclient.IZkDataListener;
import org.I0Itec.zkclient.ZkClient;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.register.server.api.ShenyuServerRegisterPublisher;
import org.apache.shenyu.register.server.api.ShenyuServerRegisterRepository;
import org.apache.shenyu.spi.Join;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * Zookeeper register center.
 */
@Slf4j
@Join
public class ZookeeperServerRegisterRepository implements ShenyuServerRegisterRepository {
    
    private ShenyuServerRegisterPublisher publisher;
    
    private ZkClient zkClient;
    
    @Override
    public void init(final ShenyuServerRegisterPublisher publisher, final ShenyuRegisterCenterConfig config) {
        this.init(config);
        this.publisher = publisher;
        Properties props = config.getProps();
        int zookeeperSessionTimeout = Integer.parseInt(props.getProperty("zookeeperSessionTimeout", "30000"));
        int zookeeperConnectionTimeout = Integer.parseInt(props.getProperty("zookeeperConnectionTimeout", "3000"));
        this.zkClient = new ZkClient(config.getServerLists(), zookeeperSessionTimeout, zookeeperConnectionTimeout);
        initSubscribe();
    }

    @Override
    public void close() {
        zkClient.close();
    }
    
    private void initSubscribe() {
        RpcTypeEnum.acquireSupportMetadatas().forEach(rpcTypeEnum -> subscribeMetaData(rpcTypeEnum.getName()));
        RpcTypeEnum.acquireSupportURIs().forEach(rpcTypeEnum -> subscribeURI(rpcTypeEnum.getName()));
    }
    
    private void subscribeURI(final String rpcType) {
        String contextPathParent = RegisterPathConstants.buildURIContextPathParent(rpcType);
        List<String> contextPaths = zkClientGetChildren(contextPathParent);
        for (String contextPath : contextPaths) {
            watcherURI(rpcType, contextPath);
        }
        zkClient.subscribeChildChanges(contextPathParent, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                for (String contextPath : currentChildren) {
                    watcherURI(rpcType, contextPath);
                }
            }
        });
    }
    
    private void subscribeMetaData(final String rpcType) {
        String contextPathParent = RegisterPathConstants.buildMetaDataContextPathParent(rpcType);
        List<String> contextPaths = zkClientGetChildren(contextPathParent);
        for (String contextPath : contextPaths) {
            watcherMetadata(rpcType, contextPath);
        }
        zkClient.subscribeChildChanges(contextPathParent, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                for (String contextPath : currentChildren) {
                    watcherMetadata(rpcType, contextPath);
                }
            }
        });
    }
    
    private void watcherMetadata(final String rpcType, final String contextPath) {
        String metaDataParentPath = RegisterPathConstants.buildMetaDataParentPath(rpcType, contextPath);
        List<String> childrenList = zkClientGetChildren(metaDataParentPath);
        if (CollectionUtils.isNotEmpty(childrenList)) {
            childrenList.forEach(children -> {
                String realPath = RegisterPathConstants.buildRealNode(metaDataParentPath, children);
                publishMetadata(zkClient.readData(realPath).toString());
                subscribeMetaDataChanges(realPath);
            });
        }
        zkClient.subscribeChildChanges(metaDataParentPath, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                List<String> addSubscribePath = addSubscribePath(childrenList, currentChildren);
                addSubscribePath.stream().map(addPath -> {
                    String realPath = RegisterPathConstants.buildRealNode(parentPath, addPath);
                    publishMetadata(zkClient.readData(realPath).toString());
                    return realPath;
                }).forEach(this::subscribeMetaDataChanges);
            
            }
        });
    }
    
    private void watcherURI(final String rpcType, final String contextPath) {
        String uriParentPath = RegisterPathConstants.buildURIParentPath(rpcType, contextPath);
        List<String> childrenList = zkClientGetChildren(uriParentPath);
        if (CollectionUtils.isNotEmpty(childrenList)) {
            registerURIChildrenList(childrenList, uriParentPath);
        }
        zkClient.subscribeChildChanges(uriParentPath, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                registerURIChildrenList(currentChildren, parentPath);
            } else {
                registerURIChildrenList(new ArrayList<>(), parentPath);
            }
        });
    }
    
    private void registerURIChildrenList(final List<String> childrenList, final String uriParentPath) {
        List<URIRegisterDTO> registerDTOList = new ArrayList<>();
        childrenList.forEach(addPath -> {
            String realPath = RegisterPathConstants.buildRealNode(uriParentPath, addPath);
            registerDTOList.add(GsonUtils.getInstance().fromJson(zkClient.readData(realPath).toString(), URIRegisterDTO.class));
        });
        if (registerDTOList.isEmpty()) {
            String contextPath = StringUtils.substringAfterLast(uriParentPath, "/");
            URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder().contextPath("/" + contextPath).build();
            registerDTOList.add(uriRegisterDTO);
        }
        publishRegisterURI(registerDTOList);
    }
    
    private void subscribeMetaDataChanges(final String realPath) {
        zkClient.subscribeDataChanges(realPath, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                publishMetadata(data.toString());
            }
        
            @SneakyThrows
            @Override
            public void handleDataDeleted(final String dataPath) {
              
            }
        });
    }
    
    private void publishMetadata(final String data) {
        publisher.publish(Lists.newArrayList(GsonUtils.getInstance().fromJson(data, MetaDataRegisterDTO.class)));
    }
    
    private void publishRegisterURI(final List<URIRegisterDTO> registerDTOList) {
        publisher.publish(registerDTOList);
    }
    
    private List<String> zkClientGetChildren(final String parent) {
        if (!zkClient.exists(parent)) {
            zkClient.createPersistent(parent, true);
        }
        return zkClient.getChildren(parent);
    }
    
    private List<String> addSubscribePath(final List<String> alreadyChildren, final List<String> currentChildren) {
        if (CollectionUtils.isEmpty(alreadyChildren)) {
            return currentChildren;
        }
        return currentChildren.stream().filter(current -> alreadyChildren.stream().noneMatch(current::equals)).collect(Collectors.toList());
    }
}
