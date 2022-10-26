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

package org.apache.shenyu.register.client.server.zookeeper;

import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.recipes.cache.ChildData;
import org.apache.curator.framework.recipes.cache.TreeCacheEvent;
import org.apache.curator.framework.recipes.cache.TreeCacheListener;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Properties;

/**
 * Zookeeper register center.
 */
@Join
public class ZookeeperClientServerRegisterRepository implements ShenyuClientServerRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ZookeeperClientServerRegisterRepository.class);

    private ShenyuClientServerRegisterPublisher publisher;

    private ZookeeperClient client;

    @Override
    public void init(final ShenyuClientServerRegisterPublisher publisher, final ShenyuRegisterCenterConfig config) {
        this.init(config);
        this.publisher = publisher;

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

        client.start();

        initSubscribe();
    }

    @Override
    public void close() {
        client.close();
    }

    private void initSubscribe() {
        RpcTypeEnum.acquireSupportMetadatas().forEach(rpcTypeEnum -> subscribeMetaData(rpcTypeEnum.getName()));
        RpcTypeEnum.acquireSupportURIs().forEach(rpcTypeEnum -> subscribeURI(rpcTypeEnum.getName()));
    }

    private void subscribeURI(final String rpcType) {
        String contextPathParent = RegisterPathConstants.buildURIContextPathParent(rpcType);
        client.addCache(contextPathParent, new URICacheListener());
    }

    private void subscribeMetaData(final String rpcType) {
        String contextPathParent = RegisterPathConstants.buildMetaDataContextPathParent(rpcType);
        client.addCache(contextPathParent, new MetadataCacheListener());
    }

    private void publishMetadata(final String data) {
        publisher.publish(Lists.newArrayList(GsonUtils.getInstance().fromJson(data, MetaDataRegisterDTO.class)));
    }

    private void publishRegisterURI(final List<URIRegisterDTO> registerDTOList) {
        publisher.publish(registerDTOList);
    }
    
    abstract static class AbstractRegisterListener implements TreeCacheListener {
        @Override
        public final void childEvent(final CuratorFramework client, final TreeCacheEvent event) {
            ChildData childData = event.getData();
            if (null == childData) {
                return;
            }
            String path = childData.getPath();
            if (Strings.isNullOrEmpty(path)) {
                return;
            }
            event(event.getType(), path, childData);
        }

        /**
         * data sync event.
         *
         * @param type tree cache event type.
         * @param path tree cache event path.
         * @param data tree cache event data.
         */
        protected abstract void event(TreeCacheEvent.Type type, String path, ChildData data);
    }

    class MetadataCacheListener extends AbstractRegisterListener {
        @Override
        public void event(final TreeCacheEvent.Type type, final String path, final ChildData data) {
            // if not uri register path, return.
            if (!path.contains(RegisterPathConstants.ROOT_PATH)) {
                return;
            }
            Optional.ofNullable(data)
                    .ifPresent(e -> {
                        String str = new String(data.getData(), StandardCharsets.UTF_8);
                        publishMetadata(str);
                        LOGGER.info("zookeeper register metadata success: {}", str);
                    });
        }
    }

    class URICacheListener extends AbstractRegisterListener {
        @Override
        public void event(final TreeCacheEvent.Type type, final String path, final ChildData data) {
            // if not uri register path, return.
            if (!path.contains(RegisterPathConstants.ROOT_PATH)) {
                return;
            }
            // get children under context path
            int lastSepIndex = path.lastIndexOf(Constants.PATH_SEPARATOR);
            String contextPath = path.substring(0, lastSepIndex);
            List<String> childrenList = client.getChildren(contextPath);

            List<URIRegisterDTO> registerDTOList = new LinkedList<>();
            childrenList.forEach(addPath -> {
                String realPath = RegisterPathConstants.buildRealNode(contextPath, addPath);
                registerDTOList.add(GsonUtils.getInstance().fromJson(client.get(realPath), URIRegisterDTO.class));
            });

            if (CollectionUtils.isEmpty(registerDTOList)) {
                String[] paths = contextPath.split(Constants.PATH_SEPARATOR);
                URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder().contextPath(Constants.PATH_SEPARATOR + paths[paths.length - 1]).rpcType(paths[paths.length - 2]).build();
                registerDTOList.add(uriRegisterDTO);
            }
            publishRegisterURI(registerDTOList);
        }
    }
}
