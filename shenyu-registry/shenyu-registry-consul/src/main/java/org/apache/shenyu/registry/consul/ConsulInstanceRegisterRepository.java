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

package org.apache.shenyu.registry.consul;

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
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
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

    private final AtomicBoolean running = new AtomicBoolean(false);

    private final Map<String, Long> consulIndexes = new HashMap<>();

    private String token;

    private String waitTime;

    private String watchDelay;

    private String tags;

    private String checkTtl;

    private TtlScheduler ttlScheduler;

    private final Map<String, List<InstanceEntity>> watcherInstanceRegisterMap = new HashMap<>();

    private final Set<String> watchSelectKeySet = new HashSet<>();

    @Override
    public void init(final RegisterConfig config) {
        final Properties props = config.getProps();
        this.checkTtl = props.getProperty("checkTtl", "5");
        this.token = props.getProperty("token", "");
        this.waitTime = props.getProperty("waitTime", "30");
        this.watchDelay = props.getProperty("watchDelay", "5");
        this.tags = props.getProperty("tags");

        final String serverList = config.getServerLists();
        if (StringUtils.isBlank(serverList)) {
            throw new ShenyuException("shenyu.register.serverLists can not be null.");
        }
        final String[] addresses = serverList.split(":");
        if (addresses.length != 2) {
            throw new ShenyuException("shenyu.register.serverLists formatter is not incorrect.");
        }
        consulClient = new ConsulClient(addresses[0], Integer.parseInt(addresses[1]));
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
    public List<InstanceEntity> selectInstances(final String selectKey) {
        if (watcherInstanceRegisterMap.containsKey(selectKey)) {
            return watcherInstanceRegisterMap.get(selectKey);
        }
        this.watcherStart(selectKey);
        final List<InstanceEntity> healthServices = this.getHealthServices(selectKey, "-1");
        watcherInstanceRegisterMap.put(selectKey, healthServices);
        return healthServices;
    }

    /**
     * async to watch data change.
     *
     * @param selectKey selectKey
     */
    public void watcherStart(final String selectKey) {
        this.running.compareAndSet(false, true);
        if (!watchSelectKeySet.add(selectKey)) {
            return;
        }
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
        if (!ObjectUtils.isEmpty(healthServices)) {
            watcherInstanceRegisterMap.put(selectKey, healthServices);
        } else {
            watcherInstanceRegisterMap.remove(selectKey);
        }
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

    private URI getURI(final Map<String, String> metadata, final HealthService healthService) {
        String scheme = "http";
        for (Map.Entry<String, String> entry : metadata.entrySet()) {
            if (entry.getValue().contains("http") || entry.getValue().contains("HTTPS")) {
                scheme = "https";
                break;
            }
        }
        int port = healthService.getService().getPort();
        if (port <= 0) {
            port = "https".equals(scheme) ? 443 : 80;
        }
        String uri = String.format("%s://%s:%s", scheme, healthService.getService().getAddress(), port);
        return URI.create(uri);
    }
}
