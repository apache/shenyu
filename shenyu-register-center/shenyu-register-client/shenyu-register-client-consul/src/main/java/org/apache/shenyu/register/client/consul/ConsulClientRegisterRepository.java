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

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.agent.model.NewService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Objects;
import java.util.Properties;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;

import static org.apache.shenyu.common.constant.Constants.PATH_SEPARATOR;
import static org.apache.shenyu.common.constant.DefaultPathConstants.SELECTOR_JOIN_RULE;

import org.apache.shenyu.common.enums.RpcTypeEnum;
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

@Join
public class ConsulClientRegisterRepository implements ShenyuClientRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ConsulClientRegisterRepository.class);

    private static final char SEPARATOR = '-';

    private ConsulClient consulClient;

    private NewService service;

    public ConsulClientRegisterRepository() { }

    public ConsulClientRegisterRepository(final ShenyuRegisterCenterConfig config) {
        init(config);
    }

    @Override
    public void init(final ShenyuRegisterCenterConfig config) {
        final Properties properties = config.getProps();
        final String serverList = config.getServerLists();
        if (StringUtils.isBlank(serverList)) {
            throw new ShenyuException("serverList can not be null.");
        }
        final String[] addresses = splitAndCheckAddress(serverList);
        consulClient = new ConsulClient(addresses[0], Integer.parseInt(addresses[1]));
        service = new NewService();
        service.setMeta(new HashMap<>());

        final String appName = properties.getProperty("name");
        service.setName(normalizeForDns(appName));
        final String instanceId = properties.getProperty("instanceId");
        service.setId(normalizeForDns(instanceId));
        final boolean preferAgentAddress = Boolean.parseBoolean(properties.getProperty("preferAgentAddress", "false"));
        if (!preferAgentAddress) {
            service.setAddress(properties.getProperty("hostName"));
        }
        final String tags = properties.getProperty("tags");
        if (StringUtils.isNotBlank(tags)) {
            service.setTags(new ArrayList<>(Arrays.asList(tags.split(","))));
        }
        service.setEnableTagOverride(Boolean.valueOf(properties.getProperty("enableTagOverride", "false")));

        final String port = properties.getProperty("port");
        if (StringUtils.isNotBlank(port)) {
            service.setPort(Integer.parseInt(port));
        }
    }

    private String[] splitAndCheckAddress(final String serverList) {
        final String[] addresses = serverList.split(":");
        if (addresses.length != 2) {
            throw new ShenyuException("serverList formatter is not incorrect.");
        }
        return addresses;
    }

    private String normalizeForDns(final String s) {
        if (s == null || !Character.isLetter(s.charAt(0))
                || !Character.isLetterOrDigit(s.charAt(s.length() - 1))) {
            throw new IllegalArgumentException(
                    "Consul service ids must not be empty, must start "
                            + "with a letter, end with a letter or digit, "
                            + "and have as interior characters only letters, "
                            + "digits, and hyphen: " + s);
        }

        StringBuilder normalized = new StringBuilder();
        Character prev = null;
        for (char curr : s.toCharArray()) {
            Character toAppend = null;
            if (Character.isLetterOrDigit(curr)) {
                toAppend = curr;
            } else if (prev == null || !(prev == SEPARATOR)) {
                toAppend = SEPARATOR;
            }
            if (toAppend != null) {
                normalized.append(toAppend);
                prev = toAppend;
            }
        }

        return normalized.toString();
    }

    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        registerMetadata(metadata);
        LogUtils.info(LOGGER, "{} Consul client register success: {}", metadata.getRpcType(), metadata);
    }

    /**
     * Persist uri.
     *
     * @param registerDTO the register dto
     */
    @Override
    public void persistURI(final URIRegisterDTO registerDTO) {
        registerURI(registerDTO);
        LogUtils.info(LOGGER, "{} Consul client register success: {}", registerDTO.getRpcType(), registerDTO);
    }

    @Override
    public void close() {
        consulClient.agentServiceDeregister(this.service.getId());
    }

    private void registerMetadata(final MetaDataRegisterDTO metadata) {
        String rpcType = metadata.getRpcType();
        String contextPath = ContextPathUtils.buildRealNode(metadata.getContextPath(), metadata.getAppName());
        String metadataNodeName = buildMetadataNodeName(metadata);
        String metaDataPath = RegisterPathConstants.buildMetaDataParentPath(rpcType, contextPath);
        String realNode = RegisterPathConstants.buildRealNode(metaDataPath, metadataNodeName);
        String metadataJson = GsonUtils.getInstance().toJson(metadata);
        consulClient.setKVValue(realNode, metadataJson);
    }

    private void registerURI(final URIRegisterDTO metadata) {
        this.service.getMeta().put(Constants.URI, GsonUtils.getInstance().toJson(metadata));
        consulClient.agentServiceRegister(this.service);
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
