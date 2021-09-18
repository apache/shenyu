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

package org.apache.shenyu.register.client.consul;

import com.ecwid.consul.v1.kv.KeyValueClient;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.LogUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.consul.serviceregistry.ConsulRegistration;

import java.util.Optional;

@Join
public class ConsulClientRegisterRepository implements ShenyuClientRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ConsulClientRegisterRepository.class);

    @Autowired
    private ConsulRegistration consulRegistration;
    
    @Autowired
    private KeyValueClient keyValueClient;
    
    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        String rpcType = metadata.getRpcType();
        String contextPath = metadata.getContextPath().substring(1);
        registerMetadata(rpcType, contextPath, metadata);
        Optional.of(RpcTypeEnum.acquireSupportURIs().stream().filter(rpcTypeEnum -> rpcType.equals(rpcTypeEnum.getName())).findFirst())
                .ifPresent(rpcTypeEnum -> {
                    registerURI(metadata);
                });
        LogUtils.info(LOGGER, "{} Consul client register success: {}", rpcType, metadata);
    }
    
    private void registerMetadata(final String rpcType, final String contextPath, final MetaDataRegisterDTO metadata) {
        String metadataNodeName = buildMetadataNodeName(metadata);
        String metaDataPath = RegisterPathConstants.buildMetaDataParentPath(rpcType, contextPath);
        
        String realNode = RegisterPathConstants.buildRealNode(metaDataPath, metadataNodeName);
        String metadataJson = GsonUtils.getInstance().toJson(metadata);
        keyValueClient.setKVValue(realNode, metadataJson);
    }
    
    private void registerURI(final MetaDataRegisterDTO metadata) {
        URIRegisterDTO uriRegisterDTO = URIRegisterDTO.transForm(metadata);
        consulRegistration.getService().getMeta().put("uri", GsonUtils.getInstance().toJson(uriRegisterDTO));
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
