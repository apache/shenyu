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
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.ContextPathUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.LogUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.Properties;


/**
 * apollo register center client.
 */
@Join
public class ApolloClientRegisterRepository implements ShenyuClientRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ApolloClientRegisterRepository.class);

    private ApolloClient apolloClient;

    @Override
    public void init(final ShenyuRegisterCenterConfig config) {
        Properties properties = config.getProps();
        String appId = properties.getProperty("appId");
        String token = properties.getProperty("token");
        String env = properties.getProperty("env", "DEV");
        String clusterName = properties.getProperty("clusterName", ConfigConsts.CLUSTER_NAME_DEFAULT);
        String namespace = properties.getProperty("namespace", ConfigConsts.NAMESPACE_APPLICATION);
        String portalUrl = properties.getProperty("portalUrl");
        String operator = properties.getProperty("operator", "apollo");

        ApolloConfig apolloConfig = new ApolloConfig();
        apolloConfig.setAppId(appId);
        apolloConfig.setPortalUrl(portalUrl);
        apolloConfig.setToken(token);
        apolloConfig.setEnv(env);
        apolloConfig.setClusterName(clusterName);
        apolloConfig.setNamespace(namespace);
        apolloConfig.setOperator(operator);

        this.apolloClient = new ApolloClient(apolloConfig);
    }

    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        registerMetadata(metadata);
        LogUtils.info(LOGGER, "{} apollo client register metadata success: {}", metadata.getRpcType(), metadata);
    }

    @Override
    public void persistURI(final URIRegisterDTO registerDTO) {
        String rpcType = registerDTO.getRpcType();
        String contextPath = ContextPathUtils.buildRealNode(registerDTO.getContextPath(), registerDTO.getAppName());
        registerURI(rpcType, contextPath, registerDTO);
        LogUtils.info(LOGGER, "{} apollo client register uri success: {}", rpcType, registerDTO);
    }

    @Override
    public void offline(final URIRegisterDTO offlineDTO) {
        String rpcType = offlineDTO.getRpcType();
        String contextPath = ContextPathUtils.buildRealNode(offlineDTO.getContextPath(), offlineDTO.getAppName());
        unRegister(rpcType, contextPath, offlineDTO);
        LogUtils.info(LOGGER, "{} apollo client unRegister uri success: {}", rpcType, offlineDTO);
    }

    private void registerURI(final String rpcType,
                             final String contextPath,
                             final URIRegisterDTO registerDTO) {
        String uriNodeName = buildURINodeName(registerDTO);
        String uriPath = RegisterPathConstants.buildURIParentPath(rpcType, contextPath);
        String realNode = RegisterPathConstants.buildRealNode(uriPath, uriNodeName);
        apolloClient.createOrUpdateItem(realNode, GsonUtils.getInstance().toJson(registerDTO), "register uri");
        apolloClient.publishNamespace("publish config", "");
        LOGGER.info("register uri data success: {}", realNode);
    }

    private void unRegister(final String rpcType,
                            final String contextPath,
                            final URIRegisterDTO offlineDTO) {
        String uriNodeName = buildURINodeName(offlineDTO);
        String uriPath = RegisterPathConstants.buildURIParentPath(rpcType, contextPath);
        String realNode = RegisterPathConstants.buildRealNode(uriPath, uriNodeName);
        apolloClient.createOrUpdateItem(realNode, GsonUtils.getInstance().toJson(offlineDTO), "offline uri");
        apolloClient.publishNamespace("publish config", "");
        LOGGER.info("unRegister uri data success: {}", realNode);
    }

    private String buildURINodeName(final URIRegisterDTO registerDTO) {
        String host = registerDTO.getHost();
        int port = registerDTO.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    private void registerMetadata(final MetaDataRegisterDTO metadata) {
        String rpcType = metadata.getRpcType();
        String contextPath = ContextPathUtils.buildRealNode(metadata.getContextPath(), metadata.getAppName());
        String metadataNodeName = RegisterPathConstants.buildNodeName(metadata.getServiceName(), metadata.getMethodName());
        String metaDataPath = RegisterPathConstants.buildMetaDataParentPath(rpcType, contextPath);
        String realNode = RegisterPathConstants.buildRealNode(metaDataPath, metadataNodeName);

        String oldValue = apolloClient.getItemValue(realNode);
        // no change in metadata, no need to update
        if (oldValue != null) {
            MetaDataRegisterDTO oldMetaData = GsonUtils.getInstance().fromJson(oldValue, MetaDataRegisterDTO.class);
            if (Objects.equals(oldMetaData, metadata)) {
                return;
            }
        }
        // update metadata
        String metadataJson = GsonUtils.getInstance().toJson(metadata);
        this.apolloClient.createOrUpdateItem(realNode, metadataJson, "register config");
        this.apolloClient.publishNamespace("publish config", "");
    }

}
