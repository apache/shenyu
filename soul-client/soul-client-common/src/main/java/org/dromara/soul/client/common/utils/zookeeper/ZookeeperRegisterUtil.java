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

package org.dromara.soul.client.common.utils.zookeeper;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import lombok.extern.slf4j.Slf4j;
import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.client.common.utils.RegisterUtil;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.constant.ZkRegisterPathConstants;
import org.dromara.soul.common.enums.RpcTypeEnum;

/**
 * zookeeper register util.
 *
 * @author lw1243925457
 */
@Slf4j
public final class ZookeeperRegisterUtil implements RegisterUtil {

    private Gson gson = new Gson();

    private ZkClient zkClient;

    private final String rootPath = ZkRegisterPathConstants.ROOT_PATH;

    private enum EnumSingleton {
        /**
         * 懒汉枚举单例.
         */
        INSTANCE;

        private ZookeeperRegisterUtil instance;

        EnumSingleton() {
            instance = new ZookeeperRegisterUtil();
        }

        public ZookeeperRegisterUtil getSingleton() {
            return instance;
        }
    }

    /**
     * get zookeeper register instance.
     *
     * @return instance
     */
    public static ZookeeperRegisterUtil getInstance() {
        return EnumSingleton.INSTANCE.getSingleton();
    }

    /**
     * set zk client.
     *
     * @param zookeeperUrl url
     * @param zookeeperSessionTimeout seesion timeout
     * @param zookeeperConnectionTimeout connection timeout
     */
    public void setZkClient(final String zookeeperUrl, final int zookeeperSessionTimeout,
                            final int zookeeperConnectionTimeout) {
        this.zkClient = new ZkClient(zookeeperUrl, zookeeperSessionTimeout, zookeeperConnectionTimeout);
    }

    @Override
    public void doRegister(final String json, final String url, final RpcTypeEnum rpcTypeEnum) {
        // TODO json string can't replace of dto?
        JsonObject registerInfo = gson.fromJson(json, JsonObject.class);
        String rpcType = registerInfo.get(Constants.RPC_TYPE).getAsString();
        String metadata = Constants.META_DATA;
        String contextPath;
        String nodeName;

        if (RpcTypeEnum.HTTP.getName().equals(rpcTypeEnum.getName())) {
            String host = registerInfo.get(Constants.HOST).getAsString();
            int port = registerInfo.get(Constants.PORT).getAsInt();
            metadata = String.join(Constants.HYPHEN, host, Integer.toString(port));
            contextPath = registerInfo.get(Constants.CONTEXT).getAsString().substring(1);
            String ruleName = buildNodeName(registerInfo.get(Constants.RULE_NAME).getAsString());
            nodeName = String.join(Constants.HYPHEN, contextPath, ruleName);
            updateUpstream(rpcType, contextPath, metadata);
        } else {
            contextPath = registerInfo.get(Constants.CONTEXT_PATH).getAsString().substring(1);
            nodeName = buildNodeName(registerInfo.get(Constants.SERVICE_NAME).getAsString(),
                                     registerInfo.get(Constants.METHOD_NAME).getAsString());
        }

        String childPath = ZkRegisterPathConstants.buildChildPath(rpcType, contextPath, metadata);
        String nodePath = ZkRegisterPathConstants.buildNodePath(childPath, nodeName);
        updateZkNode(childPath, nodePath, json);
        log.info("{} zookeeper client register success: {} : {}", rpcTypeEnum.getName(), json, nodePath);
    }

    /**
     * add uri node in http context.
     *
     * @param rpcType rpc type
     * @param contextPath context path
     * @param metadata metadata
     */
    private void updateUpstream(final String rpcType, final String contextPath, final String metadata) {
        String contextNodePath = String.join(Constants.SLASH, rootPath, rpcType, contextPath);
        if (!zkClient.exists(contextNodePath)) {
            zkClient.createPersistent(contextNodePath, true);
        }

        String metadataUriPath = String.join(Constants.SLASH, contextNodePath, metadata, "uri");
        if (!zkClient.exists(metadataUriPath)) {
            zkClient.createPersistent(metadataUriPath, true);
        }

        String uri = metadata.replace("-", ":");
        String uriNode = String.join(Constants.SLASH, metadataUriPath, uri);
        log.info("create temp uri node: {} -- {}", uriNode, uri);
        if (!zkClient.exists(uriNode)) {
            zkClient.createEphemeral(uriNode, uri);
        }
    }

    /**
     * build zookeeper node path of data.
     *
     * @param str http rule name
     * @return path
     */
    private String buildNodeName(final String str) {
        return str.substring(1).replace(Constants.SLASH, Constants.PERIOD);
    }

    /**
     * build zookeeper node path of data.
     *
     * @param serviceName rpc service class name
     * @param methodName rpc service method name
     * @return path
     */
    private String buildNodeName(final String serviceName, final String methodName) {
        return String.join(Constants.PERIOD, serviceName, methodName);
    }

    /**
     * create temp data node.
     *
     * @param childPath zk child catalog path
     * @param nodePath node path
     * @param data data
     */
    private void updateZkNode(final String childPath, final String nodePath, final String data) {
        if (!zkClient.exists(childPath)) {
            zkClient.createPersistent(childPath, true);
        }

        if (zkClient.exists(nodePath)) {
            log.error("data node path repeat: {}", nodePath);
            return;
        }
        zkClient.createEphemeral(nodePath, data);
    }
}
