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
import lombok.extern.slf4j.Slf4j;
import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.client.common.utils.RegisterUtil;
import org.dromara.soul.common.constant.ZkRegisterPathConstants;
import org.dromara.soul.common.enums.RpcTypeEnum;

import java.util.Map;

/**
 * @author lw1243925457
 */
@Slf4j
public class ZookeeperRegisterUtil implements RegisterUtil {

    private enum EnumSingleton {
        /**
         * 懒汉枚举单例
         */
        INSTANCE;
        private ZookeeperRegisterUtil instance;

        EnumSingleton(){
            instance = new ZookeeperRegisterUtil();
        }
        public ZookeeperRegisterUtil getSingleton(){
            return instance;
        }
    }

    public static ZookeeperRegisterUtil getInstance(){
        return EnumSingleton.INSTANCE.getSingleton();
    }

    private final Gson gson = new Gson();

    private ZkClient zkClient;

    private ZookeeperRegisterUtil() {}

    public void setZkClient(String zookeeperUrl, int zookeeperSessionTimeout, int zookeeperConnectionTimeout) {
        this.zkClient = new ZkClient(zookeeperUrl, zookeeperSessionTimeout, zookeeperConnectionTimeout);
    }

    @Override
    public void doRegister(String json, String url, RpcTypeEnum rpcTypeEnum) {
        Map jsonObject = gson.fromJson(json, Map.class);
        String nodeName = jsonObject.get("context").toString() + jsonObject.get("path").toString();
        String prefix = ZkRegisterPathConstants.buildTypePath(rpcTypeEnum.getName());
        updateZkNode(prefix, nodeName, json);
        log.info("{} client register success: {} ", rpcTypeEnum.getName(), json);
    }

    /**
     * create temp data node
     * @param prefix prefix
     * @param nodeName node name
     * @param data data
     */
    private void updateZkNode(String prefix, String nodeName, String data) {
        if (!zkClient.exists(prefix)) {
            zkClient.createPersistent(prefix, true);
        }

        String path = prefix + "/" + nodeName.replace("/", ".").substring(1);
        if (zkClient.exists(path)) {
            zkClient.delete(path);
        }

        zkClient.createEphemeral(path, data);
    }
}
