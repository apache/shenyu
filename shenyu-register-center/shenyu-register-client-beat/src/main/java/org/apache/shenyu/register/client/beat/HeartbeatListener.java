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

import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.InstanceTypeConstants;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.common.utils.SystemInfoUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.EventListener;

import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class HeartbeatListener {

    private static final Logger LOG = LoggerFactory.getLogger(HeartbeatListener.class);

    private ScheduledThreadPoolExecutor executor;

    private final ShenyuClientRegisterRepository httpClientRegisterRepository;

    private final ShenyuConfig shenyuConfig;

    public HeartbeatListener(final ShenyuClientRegisterRepository httpClientRegisterRepository, final ShenyuConfig shenyuConfig, final ServerProperties serverProperties) {
        executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-instance-task", false));
        this.httpClientRegisterRepository = httpClientRegisterRepository;
        this.shenyuConfig = shenyuConfig;
        LOG.info("Web server initialized on port {}, starting heartbeat reporter", serverProperties.getPort());
        //启动心跳任务
        executor.scheduleAtFixedRate(() -> {
            InstanceBeatInfoDTO instanceBeatInfoDTO = new InstanceBeatInfoDTO();
            instanceBeatInfoDTO.setInstancePort(serverProperties.getPort() + "");
            instanceBeatInfoDTO.setInstanceIp(IpUtils.getHost());
            instanceBeatInfoDTO.setNamespaceId(shenyuConfig.getNamespace());
            instanceBeatInfoDTO.setInstanceInfo(SystemInfoUtils.getSystemInfo());
            instanceBeatInfoDTO.setInstanceType(InstanceTypeConstants.BOOTSTRAP_INSTANCE_INFO);
            httpClientRegisterRepository.sendHeartbeat(instanceBeatInfoDTO);
            }, 0, 5, TimeUnit.SECONDS
        );
    }

    @EventListener(ContextClosedEvent.class)
    public void onShutdown() {
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