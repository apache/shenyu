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
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.config.ShenyuConfig.RegisterConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.InstanceRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.register.common.subsriber.WatcherListener;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

    private List<InstanceRegisterDTO> instanceRegisterDTOList = new ArrayList<>();

    private Map<String, WatcherListener> watcherListenerMap = new ConcurrentHashMap<>();

    @Override
    public void init(final RegisterConfig config) {
        final Properties props = config.getProps();
        final String timeout = props.getProperty("consulTimeout", "3000");
        final String ttl = props.getProperty("consulTTL", "3000");
        final String name = props.getProperty("consulName", "shenyu-gateway");
        check = new NewCheck();
        check.setName(name);
        check.setId(name);
        check.setTtl(ttl.concat("ms"));
        check.setTimeout(timeout.concat("ms"));
        consulClient = new ConsulClient(config.getServerLists());
        consulClient.agentCheckRegister(check);
        this.executor = new ScheduledThreadPoolExecutor(1,
                ShenyuThreadFactory.create("consul-config-watch", true));
        start();
    }

    @Override
    public void persistInstance(final InstanceRegisterDTO instance) {
        String instanceNodeName = buildInstanceNodeName(instance);
        String instancePath = RegisterPathConstants.buildInstanceParentPath();
        String realNode = RegisterPathConstants.buildRealNode(instancePath, instanceNodeName);
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

    private String buildInstanceNodeName(final InstanceRegisterDTO instance) {
        String host = instance.getHost();
        int port = instance.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    @Override
    public List<InstanceRegisterDTO> selectInstancesAndWatcher(final String selectKey, final WatcherListener watcherListener) {
        List<InstanceRegisterDTO> instanceRegisterDTOS = getInstanceRegisterDTOListByKey(selectKey);
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
        List<InstanceRegisterDTO> list = getInstanceRegisterDTOListByKey(RegisterPathConstants.buildInstanceParentPath());
        list.forEach(instanceRegisterDTO -> {
            if (!instanceRegisterDTOList.contains(instanceRegisterDTO)) {
                watcherListenerMap.get(RegisterPathConstants.buildInstanceParentPath()).listener(list);
                instanceRegisterDTOList = list;
                return;
            }
        });
    }

    /**
     *  getInstanceRegisterDTOListByKey.
     * @param selectKey key
     * @return return
     */
    public List<InstanceRegisterDTO> getInstanceRegisterDTOListByKey(final String selectKey) {
        Response<List<GetValue>> res = consulClient.getKVValues(selectKey);
        if (res == null || CollectionUtils.isEmpty(res.getValue())) {
            return Collections.emptyList();
        }
        List<InstanceRegisterDTO> instanceRegisterDTOS = new ArrayList<>();
        res.getValue().forEach(getValue -> {
            instanceRegisterDTOS.add(GsonUtils.getInstance().fromJson(getValue.getDecodedValue(), InstanceRegisterDTO.class));
        });
        return instanceRegisterDTOS;
    }

}
