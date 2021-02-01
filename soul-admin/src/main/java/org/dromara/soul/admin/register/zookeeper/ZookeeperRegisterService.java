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

package org.dromara.soul.admin.register.zookeeper;

import com.google.gson.Gson;
import lombok.extern.slf4j.Slf4j;
import org.I0Itec.zkclient.IZkDataListener;
import org.I0Itec.zkclient.ZkClient;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.admin.disruptor.SoulServerMetaDataRegisterEventPublisher;
import org.dromara.soul.admin.dto.MetaDataDTO;
import org.dromara.soul.admin.dto.SpringCloudRegisterDTO;
import org.dromara.soul.admin.dto.SpringMvcRegisterDTO;
import org.dromara.soul.common.constant.ZkRegisterPathConstants;
import org.dromara.soul.common.enums.RpcTypeEnum;

import java.util.List;

/**
 * @author lw1243925457
 */
@Slf4j
public class ZookeeperRegisterService {

    private static final SoulServerMetaDataRegisterEventPublisher INSTANCE = SoulServerMetaDataRegisterEventPublisher.getInstance();

    private final Gson gson = new Gson();

    private final ZkClient zkClient;

    public ZookeeperRegisterService(final ZkClient zkClient) {
        log.info("register zk start");
        this.zkClient = zkClient;
        watch();
    }

    private void watch() {
        List<String> childrenList = zkClientGetChildren(ZkRegisterPathConstants.ROOT_PATH);
        if (CollectionUtils.isNotEmpty(childrenList)) {
            childrenList.forEach(children -> {
                String realPath = ZkRegisterPathConstants.ROOT_PATH + "/" + children;
                subscribeRegisterType(realPath);
            });
        }

        zkClient.subscribeChildChanges(ZkRegisterPathConstants.ROOT_PATH, (parentPath, currentChildren) -> {
            log.info("new register type register: " + currentChildren.toString());
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                for (String registerType: currentChildren) {
                    subscribeRegisterType(parentPath + "/" + registerType);
                }
            }
        });
    }

    private void subscribeRegisterType(String realPath) {
        List<String> dataList = zkClientGetChildren(realPath);
        if (CollectionUtils.isNotEmpty(dataList)) {
            dataList.forEach(dataName -> {
                String dataPath = ZkRegisterPathConstants.buildDataPath(realPath, dataName);
                // TODO need to push register event when admin restart?
                subscribeRegisterDataChanges(dataPath);
            });
        }
    }

    private void subscribeRegisterDataChanges(String dataPath) {
        zkClient.subscribeDataChanges(dataPath, new IZkDataListener() {

            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                String type = dataPath.split("/")[2];
                register(type, data.toString());
            }

            @Override
            public void handleDataDeleted(final String dataPath) {
                log.info("delete :" + dataPath);
            }
        });
    }

    private void register(String type, String data) {
        // TODO replace if else to factory or better way?
        if (RpcTypeEnum.HTTP.getName().equals(type)) {
            SpringMvcRegisterDTO springMvcRegisterDTO = gson.fromJson(data, SpringMvcRegisterDTO.class);
            INSTANCE.publishEvent(RpcTypeEnum.HTTP.getName(), springMvcRegisterDTO);
            return;
        }
        if (RpcTypeEnum.SPRING_CLOUD.getName().equals(type)) {
            SpringCloudRegisterDTO springCloudRegisterDTO = gson.fromJson(data, SpringCloudRegisterDTO.class);
            INSTANCE.publishEvent(RpcTypeEnum.SPRING_CLOUD.getName(), springCloudRegisterDTO);
            return;
        }
        if (RpcTypeEnum.DUBBO.getName().equals(type)) {
            MetaDataDTO metaDataDTO = gson.fromJson(data, MetaDataDTO.class);
            INSTANCE.publishEvent(RpcTypeEnum.DUBBO.getName(), metaDataDTO);
            return;
        }
        if (RpcTypeEnum.SOFA.getName().equals(type)) {
            MetaDataDTO metaDataDTO = gson.fromJson(data, MetaDataDTO.class);
            INSTANCE.publishEvent(RpcTypeEnum.SOFA.getName(), metaDataDTO);
            return;
        }
        if (RpcTypeEnum.TARS.getName().equals(type)) {
            MetaDataDTO metaDataDTO = gson.fromJson(data, MetaDataDTO.class);
            INSTANCE.publishEvent(RpcTypeEnum.TARS.getName(), metaDataDTO);
            return;
        }
        log.error("Can't handle the data: " + type + "::" + data);
    }

    private List<String> zkClientGetChildren(final String parent) {
        if (!zkClient.exists(parent)) {
            zkClient.createPersistent(parent, true);
        }
        return zkClient.getChildren(parent);
    }
}
