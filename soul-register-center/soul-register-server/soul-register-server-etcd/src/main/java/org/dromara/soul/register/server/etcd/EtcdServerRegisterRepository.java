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

package org.dromara.soul.register.server.etcd;

import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.register.common.config.SoulRegisterCenterConfig;
import org.dromara.soul.register.common.dto.MetaDataRegisterDTO;
import org.dromara.soul.register.common.dto.URIRegisterDTO;
import org.dromara.soul.register.server.api.SoulServerRegisterPublisher;
import org.dromara.soul.register.server.api.SoulServerRegisterRepository;
import org.dromara.soul.register.server.etcd.client.EtcdClient;
import org.dromara.soul.register.server.etcd.client.EtcdListenHandler;
import org.dromara.soul.spi.Join;

import java.util.EnumSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.HashSet;
import java.util.ArrayList;


/**
 * etcd sever register repository.
 *
 * @author lw1243925457
 */
@Join
@Slf4j
public class EtcdServerRegisterRepository implements SoulServerRegisterRepository {

    private static final EnumSet<RpcTypeEnum> METADATA_SET = EnumSet.of(RpcTypeEnum.DUBBO, RpcTypeEnum.GRPC, RpcTypeEnum.HTTP, RpcTypeEnum.SPRING_CLOUD, RpcTypeEnum.SOFA, RpcTypeEnum.TARS);

    private static final EnumSet<RpcTypeEnum> URI_SET = EnumSet.of(RpcTypeEnum.GRPC, RpcTypeEnum.HTTP, RpcTypeEnum.TARS);

    private static final String METADATA_ROOT = "/soul/register/metadata";

    private static final String URI_ROOT = "/soul/register/uri";

    private SoulServerRegisterPublisher publisher;

    private EtcdClient client;

    @Override
    public void init(final SoulServerRegisterPublisher publisher, final SoulRegisterCenterConfig config) {
        Properties props = config.getProps();
        long timeout = Long.parseLong(props.getProperty("etcdTimeout", "3000"));
        long ttl = Long.parseLong(props.getProperty("etcdTTL", "5"));
        this.client = new EtcdClient(config.getServerLists(), ttl, timeout);
        this.publisher = publisher;
        initSubscribe();
    }

    @Override
    public void close() {
        client.close();
    }

    private void initSubscribe() {
        METADATA_SET.forEach(rpcTypeEnum -> subscribeMetaData(rpcTypeEnum.getName()));
        URI_SET.forEach(rpcTypeEnum -> subscribeURI(rpcTypeEnum.getName()));
    }

    private void subscribeMetaData(final String rpcType) {
        String rpcPath = String.join("/", METADATA_ROOT, rpcType);
        List<String> metadataPaths = client.getChildren(rpcPath);
        for (String metadataPath: metadataPaths) {
            String data = client.read(metadataPath);
            publishMetadata(data);
        }

        log.info("subscribe metadata change: {}", rpcPath);
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
        String rpcPath = String.join("/", URI_ROOT, rpcType);
        Set<String> contextList = new HashSet<>();
        client.getChildren(rpcPath).forEach(dataPath -> {
            String context = dataPath.split("/")[4];
            contextList.add(context);
        });

        contextList.forEach(context -> {
            registerUriChildrenList(rpcPath, context);
        });

        log.info("subscribe uri change: {}", rpcPath);
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
        client.getChildren(contextPath).forEach(path -> {
            uriRegisterDTOList.add(GsonUtils.getInstance().fromJson(client.read(path), URIRegisterDTO.class));
        });
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

