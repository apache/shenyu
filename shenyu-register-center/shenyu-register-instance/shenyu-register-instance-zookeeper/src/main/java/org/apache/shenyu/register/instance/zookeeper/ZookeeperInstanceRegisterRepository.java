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

package org.apache.shenyu.register.instance.zookeeper;

import org.apache.commons.lang3.StringUtils;
import org.apache.curator.framework.state.ConnectionState;
import org.apache.shenyu.common.config.ShenyuConfig.InstanceConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.InstanceRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.spi.Join;
import org.apache.zookeeper.CreateMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * The type Zookeeper instance register repository.
 */
@Join
public class ZookeeperInstanceRegisterRepository implements ShenyuInstanceRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ZookeeperInstanceRegisterRepository.class);

    private ZookeeperClient client;

    private final Map<String, String> nodeDataMap = new HashMap<>();

    @Override
    public void init(final InstanceConfig config) {
        Properties props = config.getProps();
        int sessionTimeout = Integer.parseInt(props.getProperty("sessionTimeout", "3000"));
        int connectionTimeout = Integer.parseInt(props.getProperty("connectionTimeout", "3000"));

        int baseSleepTime = Integer.parseInt(props.getProperty("baseSleepTime", "1000"));
        int maxRetries = Integer.parseInt(props.getProperty("maxRetries", "3"));
        int maxSleepTime = Integer.parseInt(props.getProperty("maxSleepTime", String.valueOf(Integer.MAX_VALUE)));

        ZookeeperConfig zkConfig = new ZookeeperConfig(config.getServerLists());
        zkConfig.setBaseSleepTimeMilliseconds(baseSleepTime)
                .setMaxRetries(maxRetries)
                .setMaxSleepTimeMilliseconds(maxSleepTime)
                .setSessionTimeoutMilliseconds(sessionTimeout)
                .setConnectionTimeoutMilliseconds(connectionTimeout);

        String digest = props.getProperty("digest");
        if (!StringUtils.isEmpty(digest)) {
            zkConfig.setDigest(digest);
        }

        this.client = new ZookeeperClient(zkConfig);
        this.client.getClient().getConnectionStateListenable().addListener((c, newState) -> {
            if (newState == ConnectionState.RECONNECTED) {
                nodeDataMap.forEach((k, v) -> {
                    if (!client.isExist(k)) {
                        client.createOrUpdate(k, v, CreateMode.EPHEMERAL);
                        LOGGER.info("zookeeper client register instance success: {}", v);
                    }
                });
            }
        });

        client.start();
    }
    
    @Override
    public void persistInstance(final InstanceRegisterDTO instance) {
        String uriNodeName = buildInstanceNodeName(instance);
        String instancePath = RegisterPathConstants.buildInstanceParentPath();
        if (!client.isExist(instancePath)) {
            client.createOrUpdate(instancePath, "", CreateMode.PERSISTENT);
        }
        String realNode = RegisterPathConstants.buildRealNode(instancePath, uriNodeName);
        String nodeData = GsonUtils.getInstance().toJson(instance);
        nodeDataMap.put(realNode, nodeData);
        client.createOrUpdate(realNode, nodeData, CreateMode.EPHEMERAL);
    }
    
    @Override
    public void close() {
        client.close();
    }

    private String buildInstanceNodeName(final InstanceRegisterDTO instance) {
        String host = instance.getHost();
        int port = instance.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

}
