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

package org.apache.shenyu.register.client.apollo;

import com.ctrip.framework.apollo.core.ConfigConsts;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import static org.apache.shenyu.common.constant.Constants.PATH_SEPARATOR;

/**
 * apollo register center client.
 */
@Join
public class ApolloClientRegisterRepository implements ShenyuClientRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ApolloClientRegisterRepository.class);

    private final Set<String> metadataSet = new HashSet<>();

    private ApolloClient apolloClient;

    @Override
    public void init(final ShenyuRegisterCenterConfig config) {
        Properties properties = config.getProps();
        String appId = properties.getProperty("appId");
        String portalUrl = properties.getProperty("portalUrl");
        String token = properties.getProperty("token");
        String env = properties.getProperty("env", "DEV");
        String clusterName = properties.getProperty("clusterName", ConfigConsts.CLUSTER_NAME_DEFAULT);
        String namespace = properties.getProperty("namespace", ConfigConsts.NAMESPACE_APPLICATION);

        ApolloConfig apolloConfig = new ApolloConfig();
        apolloConfig.setAppId(appId);
        apolloConfig.setPortalUrl(portalUrl);
        apolloConfig.setToken(token);
        apolloConfig.setEnv(env);
        apolloConfig.setClusterName(clusterName);
        apolloConfig.setNamespace(namespace);

        this.apolloClient = new ApolloClient(apolloConfig);
    }

    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        String rpcType = metadata.getRpcType();
        String contextPath = ContextPathUtils.buildRealNode(metadata.getContextPath(), metadata.getAppName());
        this.registerMetadata(rpcType, contextPath, metadata);
    }

    @Override
    @SuppressWarnings("unchecked")
    public void persistURI(final URIRegisterDTO registerDTO) {
        String rpcType = registerDTO.getRpcType();
        String contextPath = ContextPathUtils.buildRealNode(registerDTO.getContextPath(), registerDTO.getAppName());
        String uriPath = RegisterPathConstants.buildURIParentKey(rpcType, contextPath);
        String urlMetadata = this.apolloClient.getItemValue(uriPath);
        List<String> urlMetadataList = GsonUtils.getInstance().fromJson(urlMetadata, List.class);
        urlMetadataList = urlMetadataList == null ? new ArrayList<>() : urlMetadataList;
        urlMetadataList.add(GsonUtils.getInstance().toJson(registerDTO));
        this.apolloClient.createOrUpdateItem(uriPath, urlMetadataList, "register uri");
        this.apolloClient.publishNamespace("publish register uri", "");
    }

    private void registerMetadata(final String rpcType,
                                  final String contextPath,
                                  final MetaDataRegisterDTO metadata) {
        String metadataNodeName = buildMetadataNodeName(metadata);
        String metaDataPath = RegisterPathConstants.buildMetadataParentKey(rpcType, contextPath);
        String realNode = RegisterPathConstants.buildNodeName(metaDataPath, metadataNodeName);
        // avoid dup registration for metadata
        synchronized (metadataSet) {
            if (metadataSet.contains(realNode)) {
                return;
            }
            metadataSet.add(realNode);
        }
        this.apolloClient.createOrUpdateItem(realNode, metadata, "register metadata");
        this.apolloClient.publishNamespace("publish metadata", "");
        LOGGER.info("{} zookeeper client register metadata success: {}", rpcType, metadata);
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
}
