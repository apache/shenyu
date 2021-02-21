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

package org.dromara.soul.register.client.zookeeper;

import java.util.Properties;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import lombok.extern.slf4j.Slf4j;
import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.register.client.api.SoulClientRegisterRepository;
import org.dromara.soul.register.common.config.SoulRegisterCenterConfig;
import org.dromara.soul.register.common.dto.MetaDataDTO;
import org.dromara.soul.register.common.path.ZkRegisterPathConstants;
import org.dromara.soul.spi.Join;

/**
 * The type Zookeeper client register repository.
 *
 * @author xiaoyu
 * @author lw1243925457
 */
@Join
@Slf4j
public class ZookeeperClientRegisterRepository implements SoulClientRegisterRepository {
    
    private final String rootPath = ZkRegisterPathConstants.ROOT_PATH;
    
    private ZkClient zkClient;

    private Gson gson;

    @Override
    public void init(final SoulRegisterCenterConfig config) {
        Properties props = config.getProps();
        int zookeeperSessionTimeout = Integer.parseInt(props.getProperty("zookeeperSessionTimeout", "3000"));
        int zookeeperConnectionTimeout = Integer.parseInt(props.getProperty("zookeeperConnectionTimeout", "3000"));
        this.zkClient = new ZkClient(config.getServerLists(), zookeeperSessionTimeout, zookeeperConnectionTimeout);
        this.gson = new GsonBuilder().serializeNulls().create();
    }

    /**
     * uri(host+port)用于标识SpringMVC的下线
     *
     * @param metadata  metadata
     */
    @Override
    public void persistInterface(final MetaDataDTO metadata) {
        String rpcType = metadata.getRpcType();
        String contextPath = metadata.getContextPath().substring(1);
        String nodeName;

        if (RpcTypeEnum.HTTP.getName().equals(rpcType)) {
            String host = metadata.getHost();
            int port = metadata.getPort();
            String address = String.join(Constants.HYPHEN, host, Integer.toString(port));
            updateUpstream(rpcType, contextPath, address);
        }

        if (RpcTypeEnum.HTTP.getName().equals(rpcType) || RpcTypeEnum.SPRING_CLOUD.getName().equals(rpcType)) {
            nodeName = String.join("-", contextPath, metadata.getRuleName().replace("/", "-"));
        } else {
            nodeName = buildNodeName(metadata.getServiceName(), metadata.getMethodName());
        }

        updateZkNode(rpcType, contextPath, nodeName, metadata);
        log.info("{} zookeeper client register success: {}", rpcType, metadata.toString());
    }

    @Override
    public void close() {
        zkClient.close();
    }
    
    /**
     * add uri node in http context.
     *
     * @param rpcType rpc type
     * @param contextPath context path
     * @param metadata server address
     */
    private void updateUpstream(final String rpcType, final String contextPath, final String metadata) {
        String contextNodePath = String.join(PATH_SEPARATOR, rootPath, rpcType, contextPath);
        if (!zkClient.exists(contextNodePath)) {
            zkClient.createPersistent(contextNodePath, true);
        }
        String uri = metadata.replace("-", ":");
        String uriNode = String.join(PATH_SEPARATOR, contextNodePath, uri);
        if (!zkClient.exists(uriNode)) {
            zkClient.createEphemeral(uriNode, uri);
        }
    }
    
    private String buildNodeName(final String serviceName, final String methodName) {
        return String.join(DOT_SEPARATOR, serviceName, methodName);
    }
    
    private void updateZkNode(String rpcType, String contextPath, String nodeName, MetaDataDTO data) {
        String childPath = String.join(PATH_SEPARATOR, rootPath, rpcType, contextPath, "metadata");
        String nodePath = String.join(PATH_SEPARATOR, childPath, nodeName);

        if (!zkClient.exists(childPath)) {
            zkClient.createPersistent(childPath, true);
        }

        if (zkClient.exists(nodePath)) {
            return;
        }
        zkClient.createEphemeral(nodePath, gson.toJson(data));
    }
}
