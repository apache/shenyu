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

import lombok.extern.slf4j.Slf4j;
import org.I0Itec.zkclient.IZkDataListener;
import org.I0Itec.zkclient.ZkClient;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.admin.disruptor.SoulServerMetaDataRegisterEventPublisher;
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.listener.DataChangedEvent;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.service.SelectorService;
import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.constant.ZkRegisterPathConstants;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Collections;
import java.util.stream.Collectors;

/**
 * zookeeper register center service.
 *
 * @author lw1243925457
 */
@Slf4j
@Component
@ConditionalOnProperty(name = "soul.register.registerType.zookeeper", havingValue = "true")
public class ZookeeperRegisterService {

    private static final SoulServerMetaDataRegisterEventPublisher INSTANCE = SoulServerMetaDataRegisterEventPublisher.getInstance();
    
    private ZkClient zkClient;

    private final RpcTypeTurned rpcTypeTurned = new RpcTypeTurned();

    private final SelectorService selectorService;

    private final SelectorMapper selectorMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final Environment env;

    private final String rootPath = ZkRegisterPathConstants.ROOT_PATH;

    public ZookeeperRegisterService(final SoulClientRegisterService soulClientRegisterService,
                                    final SelectorService selectorService,
                                    final SelectorMapper selectorMapper,
                                    final ApplicationEventPublisher eventPublisher,
                                    final Environment env) {
        log.info("register zk start");
        this.selectorService = selectorService;
        this.selectorMapper = selectorMapper;
        this.eventPublisher = eventPublisher;
        this.env = env;
        INSTANCE.start(soulClientRegisterService);
        this.zkClient = initZkClient();
        subscribe(rootPath);
    }

    private ZkClient initZkClient() {
        String url = env.getProperty("soul.zookeeper.url");
        return new ZkClient(url, 5000, 2000);
    }

    /**
     * subscribe all node by recursion.
     *
     * @param path node path
     */
    private void subscribe(final String path) {
        subscribeChild(path);

        List<String> children = zkClientGetChildren(path);
        if (children.isEmpty()) {
            subscribeData(path);
            return;
        }

        children.forEach(child -> subscribe(String.join(Constants.SLASH, path, child)));
    }

    /**
     * watch child changes.
     *
     * @param path child path
     */
    private void subscribeChild(final String path) {
        zkClient.subscribeChildChanges(path, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                for (String child: currentChildren) {
                    subscribe(String.join(Constants.SLASH, parentPath, child));
                }
            }
        });
    }

    /**
     * watch data changes.
     *
     * @param dataPath data path
     */
    private void subscribeData(final String dataPath) {
        zkClient.subscribeDataChanges(dataPath, new IZkDataListener() {

            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                if (data == null) {
                    return;
                }
                log.info("data change: {} -- {}", dataPath, data.toString());
                register(getType(dataPath), data.toString());
            }

            @Override
            public void handleDataDeleted(final String dataPath) {
                if (dataPath.split(Constants.SLASH).length == 6) {
                    log.info("delete metadata : {}", dataPath);
                    zkClient.unsubscribeDataChanges(dataPath, this);
                    updateContextNode(dataPath);
                }
            }
        });

        Object data = zkClient.readData(dataPath);
        if (data != null) {
            register(getType(dataPath), data.toString());
        }
    }

    /**
     * get rpc type.
     *
     * @param dataPath path
     * @return rpc type
     */
    private String getType(final String dataPath) {
        return dataPath.split(Constants.SLASH)[3];
    }

    /**
     * register in center.
     *
     * @param type rpc type
     * @param data dto data
     */
    private void register(final String type, final String data) {
        Class clazz = rpcTypeTurned.getClass(type);
        if (clazz == null) {
            log.error("Can't handle the type data: {} -- {}", type, data);
            return;
        }
        INSTANCE.publishEvent(type, GsonUtils.getInstance().fromJson(data, clazz));
    }

    /**
     * get zk nodes of path.
     *
     * @param parent parent path
     * @return nodes
     */
    private List<String> zkClientGetChildren(final String parent) {
        if (!zkClient.exists(parent)) {
            zkClient.createPersistent(parent, true);
        }
        return zkClient.getChildren(parent);
    }

    /**
     * update divide upstream.
     *
     * @param dataPath data path
     */
    private void updateContextNode(final String dataPath) {
        List<String> path = Arrays.asList(dataPath.split("/"));
        String rpcType = path.get(3);
        String context = path.get(4);
        if (!RpcTypeEnum.HTTP.getName().equals(rpcType)) {
            return;
        }
        List<String> uriList = getNodeData(rpcType, context);
        updateSelectorHandler("/" + context, uriList);
    }

    /**
     * get uri list of context.
     *
     * @param type rpc type
     * @param context context
     * @return uri list
     */
    private List<String> getNodeData(final String type, final String context) {
        List<String> uriList = new ArrayList<>();
        String parentPath = String.join(Constants.SLASH, rootPath, type, context);
        zkClientGetChildren(parentPath).forEach(nodePath -> {
            String uri = zkClient.readData(String.join(Constants.SLASH, parentPath, nodePath));
            if (uri != null) {
                uriList.add(uri);
            }
        });
        return uriList;
    }

    /**
     * update selector and publisher event.
     *
     * @param contextPath context
     * @param uriList uri list
     */
    private void updateSelectorHandler(final String contextPath, final List<String> uriList) {
        SelectorDO selector = selectorService.findByName(contextPath);
        if (Objects.nonNull(selector)) {
            SelectorData selectorData = selectorService.buildByName(contextPath);
            if (uriList == null) {
                selector.setHandle("");
                selectorData.setHandle("");
            } else {
                String handler = GsonUtils.getInstance().toJson(buildDivideUpstream(uriList));
                selector.setHandle(handler);
                selectorData.setHandle(handler);
            }
            selectorMapper.updateSelective(selector);

            // publish change event.
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE,
                    Collections.singletonList(selectorData)));
        }
    }

    /**
     * build divide upstream list.
     *
     * @param uriList uri list
     * @return divide upstream list.
     */
    private List<DivideUpstream> buildDivideUpstream(final List<String> uriList) {
        return uriList.stream().map(uri -> DivideUpstream.builder()
                .upstreamHost("localhost")
                .protocol("http://")
                .upstreamUrl(uri)
                .weight(50)
                .build()).collect(Collectors.toList());
    }
}
