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
import org.apache.shenyu.common.exception.ShenyuException;
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

import java.util.Properties;
import java.util.concurrent.ConcurrentLinkedQueue;

/**
 * apollo register center client.
 */
@Join
public class ApolloClientRegisterRepository implements ShenyuClientRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ApolloClientRegisterRepository.class);

    private final ConcurrentLinkedQueue<String> metadataCache = new ConcurrentLinkedQueue<>();

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
        registerConfig(rpcType, contextPath, metadata);
    }

    @Override
    public void persistURI(final URIRegisterDTO registerDTO) {
        String rpcType = registerDTO.getRpcType();
        String contextPath = ContextPathUtils.buildRealNode(registerDTO.getContextPath(), registerDTO.getAppName());
        registerURI(rpcType, contextPath, registerDTO);
        LogUtils.info(LOGGER, "{} apollo client register uri success: {}", rpcType, registerDTO);
    }

    private void registerURI(final String rpcType,
                             final String contextPath,
                             final URIRegisterDTO registerDTO) {
        String uriNodeName = buildURINodeName(registerDTO);
        String uriPath = RegisterPathConstants.buildURIParentKey(rpcType, contextPath);
        String realNode = RegisterPathConstants.buildNodeName(uriPath, uriNodeName);
        apolloClient.createOrUpdateItem(realNode, GsonUtils.getInstance().toJson(registerDTO), "register uri");
        apolloClient.publishNamespace("publish config", "");
        LOGGER.info("register uri data success: {}", realNode);
    }

    private String buildURINodeName(final URIRegisterDTO registerDTO) {
        String host = registerDTO.getHost();
        int port = registerDTO.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    private synchronized void registerConfig(final String rpcType,
                                             final String contextPath,
                                             final MetaDataRegisterDTO metadata) {
        metadataCache.add(GsonUtils.getInstance().toJson(metadata));
        String configName = RegisterPathConstants.buildServiceConfigPath(rpcType, contextPath);
        try {
            this.apolloClient.createOrUpdateItem(configName, GsonUtils.getInstance().toJson(metadataCache), "register config");
            this.apolloClient.publishNamespace("publish config", "");
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }
}
