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

package org.apache.shenyu.admin.cluster;

import org.apache.shenyu.admin.service.ClusterMasterService;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.utils.IpUtils;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.integration.jdbc.lock.JdbcLockRegistry;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;

@Component
public final class SelectMasterTask implements ApplicationListener<ContextRefreshedEvent> {
    
    private static final Logger LOG = LoggerFactory.getLogger(SelectMasterTask.class);
    
    private static final String MASTER_LOCK_KEY = "shenyu:cluster:master";
    
    private final JdbcLockRegistry jdbcLockRegistry;
    
    private final ClusterMasterService clusterMasterService;
    
    private final ScheduledExecutorService executorService;
    
    private final ServerProperties serverProperties;
    
    public SelectMasterTask(final JdbcLockRegistry jdbcLockRegistry,
                            final ClusterMasterService clusterMasterService,
                            final ServerProperties serverProperties) {
        this.jdbcLockRegistry = jdbcLockRegistry;
        this.clusterMasterService = clusterMasterService;
        this.serverProperties = serverProperties;
        this.executorService = new ScheduledThreadPoolExecutor(1,
                ShenyuThreadFactory.create("master-selector", true));
    }
    
    @Override
    public void onApplicationEvent(@NotNull ContextRefreshedEvent event) {
        String host = IpUtils.getHost();
        int port = serverProperties.getPort();
        String contextPath = serverProperties.getServlet().getContextPath();
        executorService.scheduleAtFixedRate(() -> selectMaster(host, port, contextPath),
                AdminConstants.TEN_SECONDS_MILLIS_TIME,
                AdminConstants.TEN_SECONDS_MILLIS_TIME,
                TimeUnit.MILLISECONDS);
        LOG.info("select master task started, host:{}, port:{}, time: {}", host, port, LocalDateTime.now());
        
    }
    
    private void selectMaster(String host, int port, String contextPath) {
        try {
            // expires all locks older than 10 seconds
            jdbcLockRegistry.expireUnusedOlderThan(AdminConstants.TEN_SECONDS_MILLIS_TIME);
            
            // DEFAULT_TTL = 10000 ms
            Lock lock = jdbcLockRegistry.obtain(MASTER_LOCK_KEY);
            
            boolean locked = lock.tryLock(AdminConstants.FIVE_SECONDS_MILLIS_TIME,
                    TimeUnit.MILLISECONDS);
            
            if (!locked) {
                LOG.debug("select master fail, wait for next time");
                return;
            }
            clusterMasterService.selectMaster(host, String.valueOf(port), contextPath);
            
            while (true) {
                try {
                    // sleeps 10s then renew the lock
                    TimeUnit.MILLISECONDS.sleep(AdminConstants.TEN_SECONDS_MILLIS_TIME);
                    jdbcLockRegistry.renewLock(MASTER_LOCK_KEY);
                    LOG.debug("renew master lock success");
                } catch (Exception e) {
                    String message = String.format("renew master lock fail, %s", e.getMessage());
                    throw new InterruptedException(message);
                }
            }
        } catch (Exception e) {
            LOG.error("renew master lock error", e);
        }
    }
}
