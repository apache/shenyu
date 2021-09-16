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

package org.apache.shenyu.register.client.etcd;

import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.LogUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Optional;
import java.util.Properties;

/**
 * etcd register repository.
 */
@Join
public class EtcdClientRegisterRepository implements ShenyuClientRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(EtcdClientRegisterRepository.class);

    private EtcdClient client;

    @Override
    public void init(final ShenyuRegisterCenterConfig config) {
        Properties props = config.getProps();
        long timeout = Long.parseLong(props.getProperty("etcdTimeout", "3000"));
        long ttl = Long.parseLong(props.getProperty("etcdTTL", "5"));
        client = new EtcdClient(config.getServerLists(), ttl, timeout);
    }

    @Override
    public void close() {
        client.close();
    }

    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        String rpcType = metadata.getRpcType();
        String contextPath = metadata.getContextPath().substring(1);
        registerMetadata(rpcType, contextPath, metadata);
        Optional.of(RpcTypeEnum.acquireSupportURIs().stream().filter(rpcTypeEnum -> rpcType.equals(rpcTypeEnum.getName())).findFirst())
                .ifPresent(rpcTypeEnum -> {
                    registerURI(rpcType, contextPath, metadata);
                });
        LogUtils.info(LOGGER, "{} etcd client register success: {}", rpcType, metadata);
    }

    private void registerMetadata(final String rpcType, final String contextPath, final MetaDataRegisterDTO metadata) {
        String metadataNodeName = buildMetadataNodeName(metadata);
        String metaDataPath = RegisterPathConstants.buildMetaDataParentPath(rpcType, contextPath);
        String realNode = RegisterPathConstants.buildRealNode(metaDataPath, metadataNodeName);
        client.putEphemeral(realNode, GsonUtils.getInstance().toJson(metadata));
        LOGGER.info("register metadata success: {}", realNode);
    }

    private void registerURI(final String rpcType, final String contextPath, final MetaDataRegisterDTO metadata) {
        String uriNodeName = buildURINodeName(metadata);
        String uriPath = RegisterPathConstants.buildURIParentPath(rpcType, contextPath);
        String realNode = RegisterPathConstants.buildRealNode(uriPath, uriNodeName);
        client.putEphemeral(realNode, GsonUtils.getInstance().toJson(metadata));
        LOGGER.info("register uri data success: {}", realNode);
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

}
