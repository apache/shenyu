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

package org.apache.shenyu.register.client.beat;

import com.github.benmanes.caffeine.cache.CacheLoader;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.InstanceTypeConstants;
import org.apache.shenyu.common.utils.AesUtils;
import org.apache.shenyu.common.utils.BoundServerPort;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.common.utils.SystemInfoUtils;
import org.apache.shenyu.register.common.utils.RegisterUtils;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.EventListener;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class HeartbeatListener {

    private static final Logger LOG = LoggerFactory.getLogger(HeartbeatListener.class);

    private static final int INITIAL_DELAY = 0;

    private static final int CHECK_PERIOD = 5;

    private ScheduledThreadPoolExecutor executor;

    private final ShenyuConfig shenyuConfig;

    private String username;

    private String password;

    private List<String> serverList;

    /**
     * server -> accessToken.
     */
    private LoadingCache<String, String> accessToken;

    private ShenyuBootstrapHeartBeatConfig config;

    /**
     * Port of the application listener. Zero until {@link WebServerInitializedEvent}.
     */
    private volatile int boundPort;

    private volatile boolean closed;

    public HeartbeatListener(final ShenyuBootstrapHeartBeatConfig config, final ShenyuConfig shenyuConfig) {
        executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-instance-task", false));
        this.shenyuConfig = shenyuConfig;
        this.username = config.getProps().getProperty(Constants.USER_NAME);
        this.password = config.getProps().getProperty(Constants.PASS_WORD);
        this.config = config;
        String secretKey = config.getProps().getProperty(Constants.AES_SECRET_KEY);
        String secretIv = config.getProps().getProperty(Constants.AES_SECRET_IV);
        if (StringUtils.isNotBlank(secretKey) && StringUtils.isNotBlank(secretIv)) {
            this.password = AesUtils.cbcEncrypt(secretKey, secretIv, password);
        }
        this.serverList = Lists.newArrayList(Splitter.on(",").split(config.getServerLists()));
        this.accessToken = Caffeine.newBuilder()
                //see org.apache.shenyu.admin.config.properties.JwtProperties#expiredSeconds
                .expireAfterWrite(24L, TimeUnit.HOURS)
                .build(new CacheLoader<>() {
                    @Override
                    public @Nullable String load(@NonNull final String server) {
                        try {
                            Optional<?> login = RegisterUtils.doLogin(username, password, server.concat(Constants.LOGIN_PATH));
                            return login.map(String::valueOf).orElse(null);
                        } catch (Exception e) {
                            LOG.error("Login admin url :{} is fail, will retry. cause: {} ", server, e.getMessage());
                            return null;
                        }
                    }
                });
        executor.scheduleAtFixedRate(this::reportHeartbeat, INITIAL_DELAY, CHECK_PERIOD, TimeUnit.SECONDS);
    }

    /**
     * Record the application listener port. The management server is ignored, and nothing is reported
     * until a positive bound port is observed. Shutdown wins over a late initialization event.
     *
     * @param event web server initialization event
     */
    @EventListener
    public void onWebServerInitialized(final WebServerInitializedEvent event) {
        if (closed || Objects.isNull(event.getWebServer()) || Objects.isNull(event.getApplicationContext())) {
            return;
        }
        OptionalInt resolved = BoundServerPort.resolve(event.getApplicationContext().getServerNamespace(), event.getWebServer().getPort());
        if (resolved.isEmpty()) {
            LOG.debug("Ignore heartbeat port from namespace {} port {}", event.getApplicationContext().getServerNamespace(), event.getWebServer().getPort());
            return;
        }
        int observed = resolved.getAsInt();
        if (boundPort == observed) {
            return;
        }
        boundPort = observed;
        LOG.info("Application server bound on port {}, heartbeat reporter will use it", observed);
    }

    private void reportHeartbeat() {
        if (closed) {
            return;
        }
        int port = boundPort;
        if (port <= 0) {
            return;
        }
        InstanceBeatInfoDTO instanceBeatInfoDTO = new InstanceBeatInfoDTO();
        instanceBeatInfoDTO.setInstancePort(String.valueOf(port));
        instanceBeatInfoDTO.setInstanceIp(IpUtils.getHost());
        instanceBeatInfoDTO.setNamespaceId(shenyuConfig.getNamespace());
        instanceBeatInfoDTO.setInstanceInfo(SystemInfoUtils.getSystemInfo());
        instanceBeatInfoDTO.setInstanceType(InstanceTypeConstants.BOOTSTRAP_INSTANCE_TYPE);
        sendHeartbeat(instanceBeatInfoDTO);
    }

    private void sendHeartbeat(final InstanceBeatInfoDTO instanceBeatInfoDTO) {
        for (String server : serverList) {
            String concat = server.concat(Constants.BEAT_URI_PATH);
            try {
                String accessToken = this.accessToken.get(server);
                if (StringUtils.isBlank(accessToken)) {
                    throw new NullPointerException("accessToken is null");
                }
                RegisterUtils.doHeartBeat(GsonUtils.getInstance().toJson(instanceBeatInfoDTO), concat, Constants.HEARTBEAT, accessToken);
            } catch (Exception e) {
                LOG.error("HeartBeat admin url :{} is fail, will retry.", server, e);
                // This is a periodic reporter, not a failback registration: retry on the next tick.
            }
        }
    }

    @EventListener(ContextClosedEvent.class)
    public void onShutdown() {
        closed = true;
        executor.shutdown();
        try {
            if (!executor.awaitTermination(5, TimeUnit.SECONDS)) {
                executor.shutdownNow();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
