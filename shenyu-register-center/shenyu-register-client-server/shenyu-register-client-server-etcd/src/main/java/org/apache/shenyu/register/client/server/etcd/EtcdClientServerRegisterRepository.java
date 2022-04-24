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

package org.apache.shenyu.register.client.server.etcd;

import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.client.server.etcd.client.EtcdClient;
import org.apache.shenyu.register.client.server.etcd.client.EtcdListenHandler;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.HashSet;

/**
 * etcd sever register repository.
 */
@Join
public class EtcdClientServerRegisterRepository implements ShenyuClientServerRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(EtcdClientServerRegisterRepository.class);

    private static final int CONTEXT_INDEX = 5;

    private ShenyuClientServerRegisterPublisher publisher;

    private EtcdClient client;

    @Override
    public void init(final ShenyuClientServerRegisterPublisher publisher, final ShenyuRegisterCenterConfig config) {
        this.client = new EtcdClient(config.getServerLists());
        this.publisher = publisher;
        initSubscribe();
    }

    @Override
    public void close() {
        client.close();
    }

    private void initSubscribe() {
        RpcTypeEnum.acquireSupportMetadatas().forEach(rpcTypeEnum -> subscribeMetaData(rpcTypeEnum.getName()));
        RpcTypeEnum.acquireSupportURIs().forEach(rpcTypeEnum -> subscribeURI(rpcTypeEnum.getName()));
    }

    private void subscribeMetaData(final String rpcType) {
        String rpcPath = RegisterPathConstants.buildMetaDataContextPathParent(rpcType);
        List<String> metadataPaths = client.getChildren(rpcPath);
        for (String metadataPath: metadataPaths) {
            String data = client.read(metadataPath);
            publishMetadata(data);
        }

        LOGGER.info("subscribe metadata change: {}", rpcPath);
        client.subscribeChildChanges(rpcPath, new EtcdListenHandler() {

            @Override
            public void updateHandler(final String path, final String value) {
                publishMetadata(client.read(path));
            }

            @Override
            public void deleteHandler(final String path, final String value) {

            }
        });
    }

    private void publishMetadata(final String metadataData) {
        publisher.publish(Lists.newArrayList(GsonUtils.getInstance().fromJson(metadataData, MetaDataRegisterDTO.class)));
    }

    private void subscribeURI(final String rpcType) {
        String rpcPath = RegisterPathConstants.buildURIContextPathParent(rpcType);
        Set<String> contextList = new HashSet<>();
        client.getChildren(rpcPath).forEach(dataPath -> {
            contextList.add(getContext(dataPath));
        });
        contextList.forEach(context -> registerUriChildrenList(rpcPath, context, rpcType));
        LOGGER.info("subscribe uri change: {}", rpcPath);
        client.subscribeChildChanges(rpcPath, new EtcdListenHandler() {

            @Override
            public void updateHandler(final String path, final String value) {
                registerUriChildrenList(rpcPath, getContext(path), rpcType);
            }

            @Override
            public void deleteHandler(final String path, final String value) {
                registerUriChildrenList(rpcPath, getContext(path), rpcType);
            }
        });
    }

    private void registerUriChildrenList(final String rpcPath, final String context, final String rpcType) {
        String contextPath = String.join(Constants.PATH_SEPARATOR, rpcPath, context);
        List<URIRegisterDTO> uriRegisterDTOList = new LinkedList<>();
        client.getChildren(contextPath).forEach(path -> uriRegisterDTOList.add(GsonUtils.getInstance().fromJson(client.read(path), URIRegisterDTO.class)));
        if (CollectionUtils.isEmpty(uriRegisterDTOList)) {
            URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder().contextPath(Constants.PATH_SEPARATOR + context).rpcType(rpcType).build();
            uriRegisterDTOList.add(uriRegisterDTO);
        }
        publishURI(uriRegisterDTOList);
    }

    private void publishURI(final List<URIRegisterDTO> registerDTOList) {
        publisher.publish(registerDTOList);
    }

    private String getContext(final String path) {
        String[] paths = path.split(Constants.PATH_SEPARATOR);
        return paths[path.startsWith(Constants.PATH_SEPARATOR) ? CONTEXT_INDEX : (CONTEXT_INDEX - 1)];
    }
}

