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
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.ContextPathUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.LogUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.consul.serviceregistry.ConsulRegistration;

import java.util.Objects;

import static org.apache.shenyu.common.constant.Constants.PATH_SEPARATOR;
import static org.apache.shenyu.common.constant.DefaultPathConstants.SELECTOR_JOIN_RULE;

@Join
public class ConsulClientRegisterRepository implements ShenyuClientRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ConsulClientRegisterRepository.class);

    private final ConsulRegistration consulRegistration;

    private final KeyValueClient keyValueClient;

    public ConsulClientRegisterRepository(final ConsulRegistration consulRegistration,
                                          final KeyValueClient keyValueClient) {
        this.consulRegistration = consulRegistration;
        this.keyValueClient = keyValueClient;
    }

    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        String rpcType = metadata.getRpcType();
        String contextPath = ContextPathUtils.buildRealNode(metadata.getContextPath(), metadata.getAppName());
        registerMetadata(rpcType, contextPath, metadata);
        LogUtils.info(LOGGER, "{} Consul client register success: {}", rpcType, metadata);
    }
    
    /**
     * Persist uri.
     *
     * @param registerDTO the register dto
     */
    @Override
    public void persistURI(final URIRegisterDTO registerDTO) {
        registerURI(registerDTO);
    }
    
    private void registerMetadata(final String rpcType,
                                  final String contextPath,
                                  final MetaDataRegisterDTO metadata) {
        String metadataNodeName = buildMetadataNodeName(metadata);
        String metaDataPath = RegisterPathConstants.buildMetaDataParentPath(rpcType, contextPath);
        String realNode = RegisterPathConstants.buildRealNode(metaDataPath, metadataNodeName);
        String metadataJson = GsonUtils.getInstance().toJson(metadata);
        keyValueClient.setKVValue(realNode, metadataJson);
    }
    
    private void registerURI(final URIRegisterDTO metadata) {
        consulRegistration.getService().getMeta().put(Constants.URI, GsonUtils.getInstance().toJson(metadata));
    }
    
    private String buildMetadataNodeName(final MetaDataRegisterDTO metadata) {
        String nodeName;
        String rpcType = metadata.getRpcType();
        if (Objects.equals(RpcTypeEnum.HTTP.getName(), rpcType)
                || Objects.equals(RpcTypeEnum.SPRING_CLOUD.getName(), rpcType)) {
            nodeName = String.join(SELECTOR_JOIN_RULE,
                    metadata.getContextPath(),
                    metadata.getRuleName().replace(PATH_SEPARATOR, SELECTOR_JOIN_RULE));
        } else {
            nodeName = RegisterPathConstants.buildNodeName(metadata.getServiceName(), metadata.getMethodName());
        }
        return nodeName.startsWith(PATH_SEPARATOR) ? nodeName.substring(1) : nodeName;
    }
}
