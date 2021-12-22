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

import org.I0Itec.zkclient.IZkStateListener;
import org.I0Itec.zkclient.ZkClient;
import org.apache.shenyu.common.config.ShenyuConfig.InstanceConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.InstanceRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.spi.Join;
import org.apache.zookeeper.Watcher;
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

    private ZkClient zkClient;

    private final Map<String, String> nodeDataMap = new HashMap<>();

    @Override
    public void init(final InstanceConfig config) {
        Properties props = config.getProps();
        int sessionTimeout = Integer.parseInt(props.getProperty("sessionTimeout", "3000"));
        int connectionTimeout = Integer.parseInt(props.getProperty("connectionTimeout", "3000"));
        this.zkClient = new ZkClient(config.getServerLists(), sessionTimeout, connectionTimeout);
        this.zkClient.subscribeStateChanges(new ZkStateListener());
    }
    
    @Override
    public void persistInstance(final InstanceRegisterDTO instance) {
        String uriNodeName = buildInstanceNodeName(instance);
        String instancePath = RegisterPathConstants.buildInstanceParentPath();
        if (!zkClient.exists(instancePath)) {
            zkClient.createPersistent(instancePath, true);
        }
        String realNode = RegisterPathConstants.buildRealNode(instancePath, uriNodeName);
        if (!zkClient.exists(realNode)) {
            String nodeData = GsonUtils.getInstance().toJson(instance);
            nodeDataMap.put(realNode, nodeData);
            zkClient.createEphemeral(realNode, nodeData);
        }
    }
    
    @Override
    public void close() {
        zkClient.close();
    }

    private String buildInstanceNodeName(final InstanceRegisterDTO instance) {
        String host = instance.getHost();
        int port = instance.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    private class ZkStateListener implements IZkStateListener {
        @Override
        public void handleStateChanged(final Watcher.Event.KeeperState keeperState) {
            if (Watcher.Event.KeeperState.SyncConnected.equals(keeperState)) {
                nodeDataMap.forEach((k, v) -> {
                    if (!zkClient.exists(k)) {
                        zkClient.createEphemeral(k, v);
                        LOGGER.info("zookeeper client register success: {}", v);
                    }
                });
            }
        }

        @Override
        public void handleNewSession() {
        }

        @Override
        public void handleSessionEstablishmentError(final Throwable throwable) {
        }
    }
}
