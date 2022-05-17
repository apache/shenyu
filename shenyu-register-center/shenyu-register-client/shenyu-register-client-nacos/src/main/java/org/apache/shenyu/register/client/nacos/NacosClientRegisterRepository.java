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

package org.apache.shenyu.register.client.nacos;

import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.api.config.ConfigFactory;
import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.exception.NacosException;
import com.alibaba.nacos.api.naming.NamingFactory;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.naming.pojo.Instance;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentLinkedQueue;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.exception.ShenyuException;
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

/**
 * nacos register center client.
 */
@Join
public class NacosClientRegisterRepository implements ShenyuClientRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(NacosClientRegisterRepository.class);

    private static final String NAMESPACE = "nacosNameSpace";

    private static final String URI_META_DATA = "uriMetadata";
    
    private ConfigService configService;
    
    private NamingService namingService;

    private final ConcurrentLinkedQueue<String> metadataCache = new ConcurrentLinkedQueue<>();

    public NacosClientRegisterRepository() { }

    public NacosClientRegisterRepository(final ShenyuRegisterCenterConfig config) {
        init(config);
    }

    @Override
    public void init(final ShenyuRegisterCenterConfig config) {
        String serverAddr = config.getServerLists();
        Properties properties = config.getProps();
        Properties nacosProperties = new Properties();
        nacosProperties.put(PropertyKeyConst.SERVER_ADDR, serverAddr);

        nacosProperties.put(PropertyKeyConst.NAMESPACE, properties.getProperty(NAMESPACE));
        // the nacos authentication username
        nacosProperties.put(PropertyKeyConst.USERNAME, properties.getProperty(PropertyKeyConst.USERNAME, ""));
        // the nacos authentication password
        nacosProperties.put(PropertyKeyConst.PASSWORD, properties.getProperty(PropertyKeyConst.PASSWORD, ""));
        // access key for namespace
        nacosProperties.put(PropertyKeyConst.ACCESS_KEY, properties.getProperty(PropertyKeyConst.ACCESS_KEY, ""));
        // secret key for namespace
        nacosProperties.put(PropertyKeyConst.SECRET_KEY, properties.getProperty(PropertyKeyConst.SECRET_KEY, ""));
        try {
            this.configService = ConfigFactory.createConfigService(nacosProperties);
            this.namingService = NamingFactory.createNamingService(nacosProperties);
        } catch (NacosException e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public void close() {
        try {
            configService.shutDown();
            namingService.shutDown();
        } catch (NacosException e) {
            LOGGER.error("NacosClientRegisterRepository close error!", e);
        }
    }

    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        String rpcType = metadata.getRpcType();
        String contextPath = ContextPathUtils.buildRealNode(metadata.getContextPath(), metadata.getAppName());
        registerConfig(rpcType, contextPath, metadata);
    }
    
    /**
     * Persist uri.
     *
     * @param registerDTO the register dto
     */
    @Override
    public void persistURI(final URIRegisterDTO registerDTO) {
        String rpcType = registerDTO.getRpcType();
        String contextPath = ContextPathUtils.buildRealNode(registerDTO.getContextPath(), registerDTO.getAppName());
        String host = registerDTO.getHost();
        int port = registerDTO.getPort();
        registerService(rpcType, contextPath, host, port, registerDTO);
    }

    private synchronized void registerService(final String rpcType,
                                              final String contextPath,
                                              final String host,
                                              final int port,
                                              final URIRegisterDTO registerDTO) {
        Instance instance = new Instance();
        instance.setEphemeral(true);
        instance.setIp(host);
        instance.setPort(port);
        Map<String, String> metadataMap = new HashMap<>();
        metadataMap.put(Constants.CONTEXT_PATH, contextPath);
        metadataMap.put(URI_META_DATA, GsonUtils.getInstance().toJson(registerDTO));
        instance.setMetadata(metadataMap);

        String serviceName = RegisterPathConstants.buildServiceInstancePath(rpcType);
        try {
            namingService.registerInstance(serviceName, instance);
        } catch (NacosException e) {
            throw new ShenyuException(e);
        }
        LOGGER.info("register service uri success: {}", serviceName);
    }

    private synchronized void registerConfig(final String rpcType,
                                             final String contextPath,
                                             final MetaDataRegisterDTO metadata) {
        metadataCache.add(GsonUtils.getInstance().toJson(metadata));
        String configName = RegisterPathConstants.buildServiceConfigPath(rpcType, contextPath);
        try {
            final String defaultGroup = NacosPathConstants.GROUP;
            if (configService.publishConfig(configName, defaultGroup, GsonUtils.getInstance().toJson(metadataCache))) {
                LOGGER.info("register metadata success: {}", metadata.getRuleName());
            } else {
                throw new ShenyuException("register metadata fail , please check ");
            }
        } catch (NacosException e) {
            throw new ShenyuException(e);
        }
    }
}
