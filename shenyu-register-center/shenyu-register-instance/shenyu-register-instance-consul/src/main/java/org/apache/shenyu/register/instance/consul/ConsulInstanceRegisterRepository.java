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
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.config.ShenyuConfig.RegisterConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.InstanceRegisterDTO;
import org.apache.shenyu.register.common.subsriber.WatcherListener;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
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

    /**
     * Timeout to deregister services critical for longer than timeout (e.g. 30m). Requires consul versi.
     */
    private String healthCheckCriticalTimeout;

    /**
     * How often to perform the health check (e.g. 10s), defaults to 10s.
     */
    private String healthCheckInterval;

    /**
     * Timeout for health check (e.g. 10s).
     */
    private String healthCheckTimeout;

    /**
     * Custom health check url to override default.
     */
    private String healthCheckUrl;

    /**
     *  Whether to register an http or https service.
     */
    private String scheme;

    /**
     * Alternate server path to invoke for health checking.
     */
    private String healthCheckPath;

    private String registerServiceName;

    private String token;

    private String waitTime;

    private String watchDelay;

    private String tags;

    @Override
    public void init(final RegisterConfig config) {
        Properties props = config.getProps();
        this.healthCheckCriticalTimeout = props.getProperty("healthCheckCriticalTimeout", null);
        this.healthCheckInterval = props.getProperty("healthCheckInterval", "10s");
        this.healthCheckTimeout = props.getProperty("healthCheckTimeout", null);
        this.healthCheckUrl = props.getProperty("healthCheckUrl");
        this.scheme = props.getProperty("scheme", "http");
        this.healthCheckPath = props.getProperty("healthCheckPath", "/actuator/health");
        this.registerServiceName = props.getProperty("registerServiceName", "shenyu-bootstrap");
        this.token = props.getProperty("token", "");
        this.waitTime = props.getProperty("waitTime", "30");
        this.watchDelay = props.getProperty("watchDelay", "5");
        this.tags = props.getProperty("tags");
        final String port = props.getProperty("port", "8500");
        consulClient = new ConsulClient(config.getServerLists(), Integer.parseInt(port));
        Runtime.getRuntime().addShutdownHook(new Thread(this::close));
    }

    @Override
    public void persistInstance(final InstanceRegisterDTO instance) {
        String instanceNodeName = buildInstanceNodeName(instance);
        this.newService = new NewService();
        newService.setName(registerServiceName);
        newService.setId(String.join("-", registerServiceName, instanceNodeName));
        newService.setAddress(instance.getHost());
        newService.setPort(instance.getPort());
        newService.setCheck(createCheck(instance));
        if (StringUtils.hasText(tags)) {
            newService.setTags(new ArrayList<>(Arrays.asList(tags.split(","))));
        }
        newService.setMeta(Collections.singletonMap("nodeData", GsonUtils.getInstance().toJson(instance)));
        consulClient.agentServiceRegister(newService, token);
        LOGGER.info("consul client register success: {}", newService);
    }

    private NewService.Check createCheck(final InstanceRegisterDTO instance) {
        NewService.Check check = new NewService.Check();
        if (StringUtils.hasText(healthCheckCriticalTimeout)) {
            check.setDeregisterCriticalServiceAfter(healthCheckCriticalTimeout);
        }
        if (StringUtils.hasText(healthCheckPath)) {
            check.setHttp(String.format("%s://%s:%s%s", scheme,
                    instance.getHost(), instance.getPort(), healthCheckPath));
        } else {
            check.setHttp(healthCheckUrl);
        }
        check.setInterval(healthCheckInterval);
        check.setTimeout(healthCheckTimeout);
        return check;
    }

    @Override
    public void close() {
        consulClient.agentServiceDeregister(newService.getId(), token);
        if (this.running.compareAndSet(true, false) && !ObjectUtils.isEmpty(this.watchFutures)) {
            this.watchFutures.forEach(watchFuture -> watchFuture.cancel(true));
        }

    }

    private String buildInstanceNodeName(final InstanceRegisterDTO instance) {
        String host = instance.getHost();
        int port = instance.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    @Override
    public List<InstanceRegisterDTO> selectInstancesAndWatcher(final String selectKey, final WatcherListener watcherListener) {
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
        watchFutures.add(this.executor.scheduleWithFixedDelay(() -> this.watchConfigKeyValues(selectKey, watcherListener),
                5, Integer.parseInt(watchDelay), TimeUnit.SECONDS));
    }

    /**
     * watch key values.
     *
     * @param selectKey       selectKey
     * @param watcherListener watcherListener
     */
    public void watchConfigKeyValues(final String selectKey, final WatcherListener watcherListener) {
        if (!running.get()) {
            return;
        }
        // Long polling getHealthServices
        watcherListener.listener(this.getHealthServices(selectKey, waitTime));
    }

    /**
     * getHealthServices.
     *
     * @param selectKey key
     * @param waitTime waitTime
     * @return return
     */
    public List<InstanceRegisterDTO> getHealthServices(final String selectKey, final String waitTime) {
        final Long newIndex = Optional.ofNullable(consulIndexes.get(selectKey)).orElse(-1L);
        final HealthServicesRequest healthServicesRequest = HealthServicesRequest.newBuilder()
                .setToken(token)
                .setQueryParams(QueryParams.Builder.builder().setWaitTime(Long.parseLong(waitTime)).setIndex(newIndex).build()).build();

        Response<List<HealthService>> healthServices = consulClient.getHealthServices(selectKey, healthServicesRequest);
        consulIndexes.put(selectKey, healthServices.getConsulIndex());
        if (CollectionUtils.isEmpty(healthServices.getValue())) {
            return Collections.emptyList();
        }
        return healthServices.getValue().stream().map(healthService -> InstanceRegisterDTO.builder()
                .appName(healthService.getService().getService())
                .host(healthService.getService().getAddress())
                .port(healthService.getService().getPort())
                .build()).collect(Collectors.toList());
    }
}
