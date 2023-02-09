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
import com.ecwid.consul.v1.QueryParams;
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.agent.model.NewService;
import com.ecwid.consul.v1.health.HealthServicesRequest;
import com.ecwid.consul.v1.health.model.HealthService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.MapUtils;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.register.instance.api.config.RegisterConfig;
import org.apache.shenyu.register.instance.api.entity.InstanceEntity;
import org.apache.shenyu.register.instance.api.watcher.WatcherListener;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

/**
 * The type Consul instance register repository.
 */
@Join
public class ConsulInstanceRegisterRepository implements ShenyuInstanceRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ConsulInstanceRegisterRepository.class);

    private ConsulClient consulClient;

    private NewService newService;

    private final ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(Math.max(Runtime.getRuntime().availableProcessors(), 1),
            ShenyuThreadFactory.create("consul-config-watch", true));

    private final List<ScheduledFuture<?>> watchFutures = new ArrayList<>();

    private final Map<String, Set<WatcherListener>> listenerMap = new ConcurrentHashMap<>();

    private final Object lock = new Object();

    private final AtomicBoolean running = new AtomicBoolean(false);

    private final Map<String, Long> consulIndexes = new HashMap<>();

    private String token;

    private String waitTime;

    private String watchDelay;

    private String tags;

    private String checkTtl;

    private TtlScheduler ttlScheduler;

    @Override
    public void init(final RegisterConfig config) {
        final Properties props = config.getProps();
        this.checkTtl = props.getProperty("checkTtl", "5");
        this.token = props.getProperty("token", "");
        this.waitTime = props.getProperty("waitTime", "30");
        this.watchDelay = props.getProperty("watchDelay", "5");
        this.tags = props.getProperty("tags");
        final String port = props.getProperty("port", "8500");
        consulClient = new ConsulClient(config.getServerLists(), Integer.parseInt(port));
        this.ttlScheduler = new TtlScheduler(Integer.parseInt(checkTtl), consulClient);
        Runtime.getRuntime().addShutdownHook(new Thread(this::close));
    }

    @Override
    public void persistInstance(final InstanceEntity instance) {
        String instanceNodeName = buildInstanceNodeName(instance);
        this.newService = new NewService();
        newService.setName(instance.getAppName());
        newService.setId(String.join("-", instance.getAppName(), instanceNodeName));
        newService.setAddress(instance.getHost());
        newService.setPort(instance.getPort());
        newService.setCheck(createCheck());
        if (StringUtils.isNotEmpty(tags)) {
            newService.setTags(new ArrayList<>(Arrays.asList(tags.split(","))));
        }
        newService.setMeta(Collections.singletonMap("nodeData", GsonUtils.getInstance().toJson(instance)));
        consulClient.agentServiceRegister(newService, token);
        this.ttlScheduler.add(newService.getId());
        LOGGER.info("consul client register success: {}", newService);
    }

    private NewService.Check createCheck() {
        NewService.Check check = new NewService.Check();
        check.setTtl(checkTtl + "s");
        return check;
    }

    @Override
    public void close() {
        if (this.running.compareAndSet(true, false) && !ObjectUtils.isEmpty(this.watchFutures)) {
            this.watchFutures.forEach(watchFuture -> watchFuture.cancel(true));
        }
        if (!ObjectUtils.isEmpty(newService)) {
            consulClient.agentServiceDeregister(newService.getId(), token);
            ttlScheduler.remove(newService.getId());
        }

    }

    private String buildInstanceNodeName(final InstanceEntity instance) {
        String host = instance.getHost();
        int port = instance.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    @Override
    public List<InstanceEntity> selectInstancesAndWatcher(final String selectKey, final WatcherListener watcherListener) {
        this.watcherStart(selectKey, watcherListener);
        return this.getHealthServices(selectKey, "-1");
    }

    /**
     * async to watch data change.
     *
     * @param selectKey selectKey
     * @param watcherListener watcherListener
     */
    public void watcherStart(final String selectKey, final WatcherListener watcherListener) {
        this.running.compareAndSet(false, true);
        // avoid duplicate watchers
        Set<WatcherListener> eventListeners = listenerMap.get(selectKey);
        if (!ObjectUtils.isEmpty(eventListeners)) {
            eventListeners.add(watcherListener);
            return;
        }
        synchronized (lock) {
            eventListeners = MapUtils.computeIfAbsent(listenerMap, selectKey, s -> new HashSet<>());
        }
        eventListeners.add(watcherListener);
        watchFutures.add(this.executor.scheduleWithFixedDelay(() -> this.watchConfigKeyValues(selectKey),
                5, Integer.parseInt(watchDelay), TimeUnit.SECONDS));
    }

    /**
     * watch key values.
     *
     * @param selectKey       selectKey
     */
    public void watchConfigKeyValues(final String selectKey) {
        if (!running.get()) {
            return;
        }
        final List<InstanceEntity> healthServices = this.getHealthServices(selectKey, waitTime);
        Set<WatcherListener> eventListeners = Optional.ofNullable(listenerMap.get(selectKey)).orElse(Collections.emptySet());
        // Long polling getHealthServices
        eventListeners.forEach(eventListener -> eventListener.listener(healthServices));
    }

    /**
     * getHealthServices.
     *
     * @param selectKey key
     * @param waitTime waitTime
     * @return return
     */
    public List<InstanceEntity> getHealthServices(final String selectKey, final String waitTime) {
        final Long newIndex = Optional.ofNullable(consulIndexes.get(selectKey)).orElse(-1L);
        final HealthServicesRequest healthServicesRequest = HealthServicesRequest.newBuilder()
                .setToken(token)
                .setPassing(true)
                .setQueryParams(QueryParams.Builder.builder().setWaitTime(Long.parseLong(waitTime)).setIndex(newIndex).build()).build();

        Response<List<HealthService>> healthServices = consulClient.getHealthServices(selectKey, healthServicesRequest);
        consulIndexes.put(selectKey, healthServices.getConsulIndex());
        if (CollectionUtils.isEmpty(healthServices.getValue())) {
            return Collections.emptyList();
        }
        return healthServices.getValue().stream().map(healthService -> InstanceEntity.builder()
                .appName(healthService.getService().getService())
                .host(healthService.getService().getAddress())
                .port(healthService.getService().getPort())
                .build()).collect(Collectors.toList());
    }
}
