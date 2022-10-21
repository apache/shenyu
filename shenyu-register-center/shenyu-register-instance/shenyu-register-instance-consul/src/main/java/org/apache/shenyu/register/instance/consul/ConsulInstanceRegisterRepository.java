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

package org.apache.shenyu.register.instance.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.agent.model.NewCheck;
import com.ecwid.consul.v1.kv.model.GetValue;
import org.apache.commons.collections4.CollectionUtils;
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Consul instance register repository.
 */
@Join
public class ConsulInstanceRegisterRepository implements ShenyuInstanceRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ConsulInstanceRegisterRepository.class);

    private ConsulClient consulClient;

    private NewCheck check;

    private ScheduledThreadPoolExecutor executor;

    private ScheduledFuture<?> watchFuture;

    private final AtomicBoolean running = new AtomicBoolean(false);

    private List<InstanceEntity> instanceEntityList = new ArrayList<>();

    private final Map<String, WatcherListener> watcherListenerMap = new ConcurrentHashMap<>();

    @Override
    public void init(final RegisterConfig config) {
        final Properties props = config.getProps();
        final String timeout = props.getProperty("consulTimeout", "3000");
        final String ttl = props.getProperty("consulTTL", "3000");
        final String name = props.getProperty("consulName", "shenyu-gateway");
        final String enabledServerRebalance = props.getProperty("enabledServerRebalance", "false");
        check = new NewCheck();
        check.setName(name);
        check.setId(name);
        check.setTtl(ttl.concat("ms"));
        check.setTimeout(timeout.concat("ms"));
        consulClient = new ConsulClient(config.getServerLists());
        consulClient.agentCheckRegister(check);
        if ("true".equals(enabledServerRebalance)) {
            this.executor = new ScheduledThreadPoolExecutor(1);
            start();
        }
    }

    @Override
    public void persistInstance(final InstanceEntity instance) {
        String instanceNodeName = buildInstanceNodeName(instance);
        String instancePath = InstancePathConstants.buildInstanceParentPath();
        String realNode = InstancePathConstants.buildRealNode(instancePath, instanceNodeName);
        String nodeData = GsonUtils.getInstance().toJson(instance);
        consulClient.setKVValue(realNode, nodeData);
        LOGGER.info("consul client register success: {}", nodeData);

    }

    @Override
    public void close() {
        consulClient.agentCheckDeregister(check.getId());
        if (this.running.compareAndSet(true, false) && Objects.nonNull(this.watchFuture)) {
            this.watchFuture.cancel(true);
        }

    }

    private String buildInstanceNodeName(final InstanceEntity instance) {
        String host = instance.getHost();
        int port = instance.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    @Override
    public List<InstanceEntity> selectInstancesAndWatcher(final String selectKey, final WatcherListener watcherListener) {
        List<InstanceEntity> instanceRegisterDTOS = getInstanceRegisterDTOListByKey(selectKey);
        watcherListenerMap.put(selectKey, watcherListener);
        return instanceRegisterDTOS;
    }

    /**
     * async to watch data change.
     */
    public void start() {
        if (this.running.compareAndSet(false, true)) {
            this.watchFuture = this.executor.scheduleWithFixedDelay(this::watchConfigKeyValues,
                    5, 50, TimeUnit.MILLISECONDS);
        }
    }

    /**
     * watch key values.
     */
    public void watchConfigKeyValues() {
        if (!running.get()) {
            return;
        }
        List<InstanceEntity> list = getInstanceRegisterDTOListByKey(InstancePathConstants.buildInstanceParentPath());
        list.forEach(instanceRegisterDTO -> {
            if (!instanceEntityList.contains(instanceRegisterDTO)) {
                watcherListenerMap.get(InstancePathConstants.buildInstanceParentPath()).listener(list);
                instanceEntityList = list;
                return;
            }
        });
    }

    /**
     * getInstanceRegisterDTOListByKey.
     *
     * @param selectKey key
     * @return return
     */
    public List<InstanceEntity> getInstanceRegisterDTOListByKey(final String selectKey) {
        Response<List<GetValue>> res = consulClient.getKVValues(selectKey);
        if (res == null || CollectionUtils.isEmpty(res.getValue())) {
            return Collections.emptyList();
        }
        List<InstanceEntity> cacheInstanceRegisters = new ArrayList<>();
        res.getValue().forEach(getValue -> {
            cacheInstanceRegisters.add(GsonUtils.getInstance().fromJson(getValue.getDecodedValue(), InstanceEntity.class));
        });
        return cacheInstanceRegisters;
    }

    @Override
    public List<InstanceEntity> getInstanceRegisterList() {
        return instanceEntityList;
    }
}
