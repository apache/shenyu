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

package org.apache.shenyu.register.instance.etcd;

import io.etcd.jetcd.Watch;
import io.etcd.jetcd.watch.WatchEvent;
import org.apache.shenyu.common.config.ShenyuConfig.InstanceConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.InstanceRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.register.common.subsriber.WatcherListener;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Properties;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * The type Etcd instance register repository.
 */
@Join
public class EtcdInstanceRegisterRepository implements ShenyuInstanceRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(EtcdInstanceRegisterRepository.class);

    private EtcdClient client;

    @Override
    public void init(final InstanceConfig config) {
        Properties props = config.getProps();
        long timeout = Long.parseLong(props.getProperty("etcdTimeout", "3000"));
        long ttl = Long.parseLong(props.getProperty("etcdTTL", "5"));
        client = new EtcdClient(config.getServerLists(), ttl, timeout);
    }

    @Override
    public void persistInstance(final InstanceRegisterDTO instance) {
        String instanceNodeName = buildInstanceNodeName(instance);
        String instancePath = RegisterPathConstants.buildInstanceParentPath();
        String realNode = RegisterPathConstants.buildRealNode(instancePath, instanceNodeName);
        String nodeData = GsonUtils.getInstance().toJson(instance);
        client.putEphemeral(realNode, nodeData);
    }

    @Override
    public List<InstanceRegisterDTO> selectInstancesAndWatcher(final String watchKey, final WatcherListener watcherListener) {
        final Function<List<String>, List<InstanceRegisterDTO>> getInstanceRegisterFun = childrenList -> childrenList.stream().distinct().map(childPath ->
                GsonUtils.getInstance().fromJson(childPath, InstanceRegisterDTO.class)).collect(Collectors.toList());

        List<String> serverNodes = client.getKeysByPrefix(watchKey).stream().distinct().collect(Collectors.toList());

        this.client.watchKeyChanges(watchKey, Watch.listener(response -> {
            for (WatchEvent event : response.getEvents()) {
                String value = event.getKeyValue().getValue().toString(StandardCharsets.UTF_8);
                switch (event.getEventType()) {
                    case PUT:
                        serverNodes.add(value);
                        LOGGER.info("watch key {} updated, value is {}", watchKey, value);
                        continue;
                    case DELETE:
                        serverNodes.remove(value);
                        LOGGER.info("watch key {} deleted, value is {}", watchKey, value);
                        continue;
                    default:
                }
            }
            watcherListener.listener(getInstanceRegisterFun.apply(serverNodes));
        }));
        return getInstanceRegisterFun.apply(serverNodes);
    }

    private String buildInstanceNodeName(final InstanceRegisterDTO instance) {
        String host = instance.getHost();
        int port = instance.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    @Override
    public void close() {
        client.close();
    }
}
