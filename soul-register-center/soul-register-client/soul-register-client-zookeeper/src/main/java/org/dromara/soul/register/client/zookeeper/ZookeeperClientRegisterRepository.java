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
import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.register.client.api.SoulClientRegisterRepository;
import org.dromara.soul.register.common.config.SoulRegisterCenterConfig;
import org.dromara.soul.register.common.dto.MetaDataDTO;
import org.dromara.soul.register.common.path.ZkRegisterPathConstants;
import org.dromara.soul.spi.Join;

/**
 * The type Zookeeper client register repository.
 *
 * @author xiaoyu
 */
@Join
public class ZookeeperClientRegisterRepository implements SoulClientRegisterRepository {
    
    private final String rootPath = ZkRegisterPathConstants.ROOT_PATH;
    
    private ZkClient zkClient;
    
    @Override
    public void init(final SoulRegisterCenterConfig config) {
        Properties props = config.getProps();
        int zookeeperSessionTimeout = Integer.parseInt(props.getProperty("zookeeperSessionTimeout", "3000"));
        int zookeeperConnectionTimeout = Integer.parseInt(props.getProperty("zookeeperConnectionTimeout", "3000"));
        this.zkClient = new ZkClient(config.getServerLists(), zookeeperSessionTimeout, zookeeperConnectionTimeout);
    }

    @Override
    public void persistInterface(final MetaDataDTO metadata) {
    
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
     * @param metadata metadata
     */
    private void updateUpstream(final String rpcType, final String contextPath, final String metadata) {
        String contextNodePath = String.join(PATH_SEPARATOR, rootPath, rpcType, contextPath);
        if (!zkClient.exists(contextNodePath)) {
            zkClient.createPersistent(contextNodePath, true);
        }
        String metadataUriPath = String.join(PATH_SEPARATOR, contextNodePath, metadata, "uri");
        if (!zkClient.exists(metadataUriPath)) {
            zkClient.createPersistent(metadataUriPath, true);
        }
        String uri = metadata.replace("-", ":");
        String uriNode = String.join(PATH_SEPARATOR, metadataUriPath, uri);
        if (!zkClient.exists(uriNode)) {
            zkClient.createEphemeral(uriNode, uri);
        }
    }
    
    private String buildNodeName(final String str) {
        return str.substring(1).replace(PATH_SEPARATOR, DOT_SEPARATOR);
    }
    
    private String buildNodeName(final String serviceName, final String methodName) {
        return String.join(DOT_SEPARATOR, serviceName, methodName);
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
            return;
        }
        zkClient.createEphemeral(nodePath, data);
    }
}
