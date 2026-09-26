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

package org.apache.shenyu.client.core.disruptor.subcriber;

import com.google.common.base.Stopwatch;
import org.apache.shenyu.client.core.shutdown.ShenyuClientShutdownHook;
import org.apache.shenyu.client.core.shutdown.ShutdownHookManager;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.utils.SystemInfoUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

/**
 * The type Shenyu client uri executor subscriber.
 */
public class ShenyuClientURIExecutorSubscriber implements ExecutorTypeSubscriber<URIRegisterDTO> {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuClientURIExecutorSubscriber.class);

    /**
     * URIs registered through this subscriber instance only. Instance-scoped so that
     * subscriber instances from different client contexts in the same JVM never
     * heartbeat or offline each other's URIs, and re-registered URIs do not
     * accumulate duplicates in the heartbeat list.
     */
    private final List<URIRegisterDTO> uris = new CopyOnWriteArrayList<>();
    
    private final ShenyuClientRegisterRepository shenyuClientRegisterRepository;
    
    private final ScheduledThreadPoolExecutor executor;

    private final long readinessTimeoutMillis;
    
    /**
     * Instantiates a new Shenyu client uri executor subscriber.
     * URI readiness is bounded by {@code shenyu.client.uri.readyTimeoutMillis}, defaulting to three minutes.
     * Unready URIs are logged and skipped so subsequent registration events can be processed.
     *
     * @param shenyuClientRegisterRepository the shenyu client register repository
     */
    public ShenyuClientURIExecutorSubscriber(final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        this(shenyuClientRegisterRepository, Long.getLong("shenyu.client.uri.readyTimeoutMillis", TimeUnit.MINUTES.toMillis(3)));
    }

    ShenyuClientURIExecutorSubscriber(final ShenyuClientRegisterRepository shenyuClientRegisterRepository, final long readinessTimeoutMillis) {
        if (readinessTimeoutMillis <= 0) {
            throw new IllegalArgumentException("URI readiness timeout must be positive");
        }
        this.readinessTimeoutMillis = readinessTimeoutMillis;
        this.shenyuClientRegisterRepository = shenyuClientRegisterRepository;
        // executor for send heartbeat
        ThreadFactory requestFactory = ShenyuThreadFactory.create("heartbeat-reporter", true);
        executor = new ScheduledThreadPoolExecutor(1, requestFactory);
        
        executor.scheduleAtFixedRate(() -> uris.forEach(this::sendHeartbeat), 30, 10, TimeUnit.SECONDS);
    }
    
    @Override
    public DataType getType() {
        return DataType.URI;
    }
    
    @Override
    public void executor(final Collection<URIRegisterDTO> dataList) {
        for (URIRegisterDTO uriRegisterDTO : dataList) {
            if (!awaitReadiness(uriRegisterDTO)) {
                if (Thread.currentThread().isInterrupted()) {
                    return;
                }
                continue;
            }
            ShenyuClientShutdownHook.delayOtherHooks();
            shenyuClientRegisterRepository.persistURI(uriRegisterDTO);

            addUriIfAbsent(uriRegisterDTO);
            
            ShutdownHookManager.get().addShutdownHook(new Thread(() -> {
                final URIRegisterDTO offlineDTO = new URIRegisterDTO();
                BeanUtils.copyProperties(uriRegisterDTO, offlineDTO);
                offlineDTO.setEventType(EventType.OFFLINE);
                shenyuClientRegisterRepository.offline(offlineDTO);
                
                // shutdown heartbeat executor
                if (!executor.isTerminated()) {
                    executor.shutdown();
                }
            }), 2);
        }
    }

    private boolean awaitReadiness(final URIRegisterDTO uri) {
        Stopwatch stopwatch = Stopwatch.createStarted();
        while (!Thread.currentThread().isInterrupted()) {
            long remaining = readinessTimeoutMillis - stopwatch.elapsed(TimeUnit.MILLISECONDS);
            if (remaining <= 0) {
                LOG.error("Skipping URI registration for {}:{} after waiting {}ms for readiness", uri.getHost(), uri.getPort(), readinessTimeoutMillis);
                return false;
            }
            try (Socket socket = new Socket()) {
                socket.connect(new InetSocketAddress(uri.getHost(), uri.getPort()), (int) Math.min(1000, remaining));
                return true;
            } catch (IOException e) {
                LOG.debug("URI {}:{} is not ready", uri.getHost(), uri.getPort(), e);
            }
            remaining = readinessTimeoutMillis - stopwatch.elapsed(TimeUnit.MILLISECONDS);
            if (remaining > 0) {
                try {
                    TimeUnit.MILLISECONDS.sleep(Math.min(1000, remaining));
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    LOG.warn("Interrupted while waiting for URI {}:{} readiness", uri.getHost(), uri.getPort());
                    return false;
                }
            }
        }
        return false;
    }
    
    private void sendHeartbeat(final URIRegisterDTO uriRegisterDTO) {
        uriRegisterDTO.setInstanceInfo(SystemInfoUtils.getSystemInfo());
        shenyuClientRegisterRepository.sendHeartbeat(uriRegisterDTO);
    }

    private void addUriIfAbsent(final URIRegisterDTO uriRegisterDTO) {
        boolean alreadyRegistered = uris.stream().anyMatch(registered ->
                Objects.equals(registered.getNamespaceId(), uriRegisterDTO.getNamespaceId())
                        && Objects.equals(registered.getContextPath(), uriRegisterDTO.getContextPath())
                        && Objects.equals(registered.getHost(), uriRegisterDTO.getHost())
                        && Objects.equals(registered.getPort(), uriRegisterDTO.getPort()));
        if (!alreadyRegistered) {
            uris.add(uriRegisterDTO);
        }
    }
}
