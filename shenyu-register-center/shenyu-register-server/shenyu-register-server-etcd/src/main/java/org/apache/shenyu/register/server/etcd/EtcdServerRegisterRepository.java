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

package org.apache.shenyu.register.server.etcd;

import com.google.common.collect.Lists;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.register.server.api.ShenyuServerRegisterPublisher;
import org.apache.shenyu.register.server.api.ShenyuServerRegisterRepository;
import org.apache.shenyu.register.server.etcd.client.EtcdClient;
import org.apache.shenyu.register.server.etcd.client.EtcdListenHandler;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Set;
import java.util.HashSet;
import java.util.ArrayList;


/**
 * etcd sever register repository.
 */
@Join
public class EtcdServerRegisterRepository implements ShenyuServerRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(EtcdServerRegisterRepository.class);

    private ShenyuServerRegisterPublisher publisher;

    private EtcdClient client;

    @Override
    public void init(final ShenyuServerRegisterPublisher publisher, final ShenyuRegisterCenterConfig config) {
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
            String context = dataPath.split("/")[4];
            contextList.add(context);
        });

        contextList.forEach(context -> registerUriChildrenList(rpcPath, context));

        LOGGER.info("subscribe uri change: {}", rpcPath);
        client.subscribeChildChanges(rpcPath, new EtcdListenHandler() {

            @Override
            public void updateHandler(final String path, final String value) {
                String[] paths = path.split("/");
                registerUriChildrenList(rpcPath, paths[4]);
            }

            @Override
            public void deleteHandler(final String path, final String value) {
                String[] paths = path.split("/");
                registerUriChildrenList(rpcPath, paths[4]);
            }
        });
    }

    private void registerUriChildrenList(final String rpcPath, final String context) {
        String contextPath = String.join("/", rpcPath, context);
        List<URIRegisterDTO> uriRegisterDTOList = new ArrayList<>();
        client.getChildren(contextPath).forEach(path -> uriRegisterDTOList.add(GsonUtils.getInstance().fromJson(client.read(path), URIRegisterDTO.class)));
        if (uriRegisterDTOList.isEmpty()) {
            URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder().contextPath("/" + context).build();
            uriRegisterDTOList.add(uriRegisterDTO);
        }
        publishURI(uriRegisterDTOList);
    }

    private void publishURI(final List<URIRegisterDTO> registerDTOList) {
        publisher.publish(registerDTOList);
    }
}

