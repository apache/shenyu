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
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.register.instance.api.config.RegisterConfig;
import org.apache.shenyu.register.instance.api.entity.InstanceEntity;
import org.apache.shenyu.register.instance.api.path.InstancePathConstants;
import org.apache.shenyu.register.instance.api.watcher.WatcherListener;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
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
    public void init(final RegisterConfig config) {
        Properties props = config.getProps();
        long timeout = Long.parseLong(props.getProperty("etcdTimeout", "3000"));
        long ttl = Long.parseLong(props.getProperty("etcdTTL", "5"));
        client = new EtcdClient(config.getServerLists(), ttl, timeout);
    }

    @Override
    public void persistInstance(final InstanceEntity instance) {
        String instanceNodeName = buildInstanceNodeName(instance);
        String instancePath = InstancePathConstants.buildInstanceParentPath(instance.getAppName());
        String realNode = InstancePathConstants.buildRealNode(instancePath, instanceNodeName);
        String nodeData = GsonUtils.getInstance().toJson(instance);
        client.putEphemeral(realNode, nodeData);
        LOGGER.info("etcd client register success: {}", nodeData);
    }

    @Override
    public List<InstanceEntity> selectInstancesAndWatcher(final String selectKey, final WatcherListener watcherListener) {
        final String watchKey = InstancePathConstants.buildInstanceParentPath(selectKey);
        final Function<Map<String, String>, List<InstanceEntity>> getInstanceRegisterFun = childrenList ->
                childrenList.values().stream().map(x -> GsonUtils.getInstance().fromJson(x, InstanceEntity.class)).collect(Collectors.toList());
        Map<String, String> serverNodes = client.getKeysMapByPrefix(watchKey);
        this.client.watchKeyChanges(watchKey, Watch.listener(response -> {
            for (WatchEvent event : response.getEvents()) {
                String value = event.getKeyValue().getValue().toString(StandardCharsets.UTF_8);
                String path = event.getKeyValue().getKey().toString(StandardCharsets.UTF_8);
                switch (event.getEventType()) {
                    case PUT:
                        serverNodes.put(path, value);
                        LOGGER.info("watch key {} updated, value is {}", watchKey, value);
                        continue;
                    case DELETE:
                        serverNodes.remove(path);
                        LOGGER.info("watch key {} deleted, key is {}", watchKey, path);
                        continue;
                    default:
                }
            }
            watcherListener.listener(getInstanceRegisterFun.apply(serverNodes));
        }));
        return getInstanceRegisterFun.apply(serverNodes);
    }

    private String buildInstanceNodeName(final InstanceEntity instance) {
        String host = instance.getHost();
        int port = instance.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    @Override
    public void close() {
        client.close();
    }
}
