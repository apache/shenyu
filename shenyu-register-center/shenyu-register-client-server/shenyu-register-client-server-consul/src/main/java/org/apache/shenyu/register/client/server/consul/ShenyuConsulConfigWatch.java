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

package org.apache.shenyu.register.client.server.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.QueryParams;
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.kv.model.GetValue;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.SmartLifecycle;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Shenyu consul config watch.
 */
public class ShenyuConsulConfigWatch implements SmartLifecycle {

    private static final Logger LOGGER = LoggerFactory.getLogger(ShenyuConsulConfigWatch.class);

    @Resource(name = "registerConsulClient")
    private ConsulClient consul;

    private final ScheduledThreadPoolExecutor executor;

    private final int waitTime;

    private final int watchDelay;

    private final AtomicBoolean running = new AtomicBoolean(false);

    private final Map<String, Long> consulIndexes = new HashMap<>();

    private final ApplicationEventPublisher publisher;

    private ScheduledFuture<?> watchFuture;
    
    /**
     * Instantiates a new Shenyu consul config watch.
     *
     * @param config the config
     * @param publisher the publisher
     */
    public ShenyuConsulConfigWatch(final ShenyuRegisterCenterConfig config,
                                   final ApplicationEventPublisher publisher) {
        this.watchDelay = Integer.parseInt(config.getProps().getProperty("delay", "1"));
        this.waitTime = Integer.parseInt(config.getProps().getProperty("wait-time", "55"));
        executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("consul-config-watch", true));

        String metadataPath = config.getProps().getProperty("metadata-path", "shenyu/register");
        consulIndexes.put(metadataPath, 0L);

        this.publisher = publisher;
    }

    private void watchConfigKeyValues() {
        if (!this.running.get()) {
            return;
        }
        consulIndexes.keySet().forEach(context -> {
            try {
                Long currentIndex = this.consulIndexes.computeIfAbsent(context, k -> -1L);
                Response<List<GetValue>> response = this.consul.getKVValues(context, null, new QueryParams(waitTime, currentIndex));
                if (CollectionUtils.isNotEmpty(response.getValue())) {
                    Long newIndex = response.getConsulIndex();

                    if (Objects.nonNull(newIndex) && !newIndex.equals(currentIndex)) {
                        if (!this.consulIndexes.containsValue(newIndex)
                                && !currentIndex.equals(-1L)) {
                            LOGGER.trace("Context {} has new index {}", context, newIndex);
                            Map<String, GetValue> valueMap = extractGetValue(response);
                            publisher.publishEvent(new ConsulConfigChangedEvent(this, newIndex, valueMap));
                        } else if (LOGGER.isTraceEnabled()) {
                            LOGGER.info("Event for index already published for context {}", context);
                        }
                        this.consulIndexes.put(context, newIndex);
                    } else if (LOGGER.isTraceEnabled()) {
                        LOGGER.trace("Same index for context {}", context);
                    }
                } else if (LOGGER.isTraceEnabled()) {
                    LOGGER.warn("No value for context {}", context);
                }
            } catch (Exception e) {
                LOGGER.error("Error querying consul Key/Values for context '{}'. Message: {}", context, e.getMessage());
            }
        });
    }

    @Override
    public void start() {
        if (this.running.compareAndSet(false, true)) {
            this.watchFuture = this.executor.scheduleWithFixedDelay(this::watchConfigKeyValues, 5, watchDelay, TimeUnit.MILLISECONDS);
        }
    }

    @Override
    public boolean isRunning() {
        return this.running.get();
    }

    @Override
    public void stop() {
        if (this.running.compareAndSet(true, false) && this.watchFuture != null) {
            this.watchFuture.cancel(true);
        }
    }

    private Map<String, GetValue> extractGetValue(final Response<List<GetValue>> response) {
        Map<String, GetValue> valueMap = new HashMap<>();
        List<GetValue> values = response.getValue();
        values.forEach(getValue -> valueMap.put(getValue.getKey(), getValue));
        return valueMap;
    }

}
