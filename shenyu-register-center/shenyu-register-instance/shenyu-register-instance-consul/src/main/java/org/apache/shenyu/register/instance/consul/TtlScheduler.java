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
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * TtlScheduler.
 */
public class TtlScheduler {

    private static final Log log = LogFactory.getLog(TtlScheduler.class);

    private final Map<String, ScheduledFuture<?>> serviceHeartbeats = new ConcurrentHashMap<>();

    private final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor();

    private final int ttl;

    private final ConsulClient client;

    public TtlScheduler(final int ttl, final ConsulClient client) {
        this.ttl = ttl;
        this.client = client;
    }

    /**
     * Add a service to the checks loop.
     * @param instanceId instance id
     */
    public void add(final String instanceId) {
        final ScheduledFuture<?> task = this.scheduler.scheduleAtFixedRate(
                new ConsulHeartbeatTask(instanceId), ttl, ttl, TimeUnit.SECONDS);
        final ScheduledFuture<?> previousTask = this.serviceHeartbeats.put(instanceId, task);
        if (previousTask != null) {
            previousTask.cancel(true);
        }
    }

    /**
     * remove.
     *
     * @param instanceId instanceId
     */
    public void remove(final String instanceId) {
        ScheduledFuture<?> task = this.serviceHeartbeats.get(instanceId);
        if (task != null) {
            task.cancel(true);
        }
        this.serviceHeartbeats.remove(instanceId);
    }

    private class ConsulHeartbeatTask implements Runnable {

        private String checkId;

        ConsulHeartbeatTask(final String serviceId) {
            this.checkId = serviceId;
            if (!this.checkId.startsWith("service:")) {
                this.checkId = "service:" + this.checkId;
            }
        }

        @Override
        public void run() {
            TtlScheduler.this.client.agentCheckPass(this.checkId);
            if (log.isDebugEnabled()) {
                log.debug("Sending consul heartbeat for: " + this.checkId);
            }
        }
    }
}
