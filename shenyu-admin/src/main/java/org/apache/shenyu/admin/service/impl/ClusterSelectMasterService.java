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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.service.ClusterMasterService;
import org.apache.shenyu.admin.service.manager.LoadServiceDocEntry;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.integration.jdbc.lock.JdbcLockRegistry;
import org.springframework.stereotype.Service;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;

@Service
public class ClusterSelectMasterService {
    
    private static final Logger LOG = LoggerFactory.getLogger(ClusterSelectMasterService.class);
    
    private static final String MASTER_LOCK_KEY = "shenyu:cluster:master";
    
    private final ClusterMasterService clusterMasterService;
    
    private final JdbcLockRegistry jdbcLockRegistry;
    
    private final UpstreamCheckService upstreamCheckService;
    
    private final LoadServiceDocEntry loadServiceDocEntry;
    
    private final ScheduledExecutorService executorService;
    
    public ClusterSelectMasterService(final JdbcLockRegistry jdbcLockRegistry,
                                      final ClusterMasterService clusterMasterService,
                                      final UpstreamCheckService upstreamCheckService,
                                      final LoadServiceDocEntry loadServiceDocEntry) {
        this.jdbcLockRegistry = jdbcLockRegistry;
        this.clusterMasterService = clusterMasterService;
        this.upstreamCheckService = upstreamCheckService;
        this.loadServiceDocEntry = loadServiceDocEntry;
        this.executorService = new ScheduledThreadPoolExecutor(1,
                ShenyuThreadFactory.create("master-selector", true));
    }
    
    /**
     * start master select task.
     * @param host host
     * @param port port
     * @param contextPath contextPath
     */
    public void startSelectMasterTask(final String host, final String port, final String contextPath) {
        LOG.info("starting select master task");
        executorService.scheduleAtFixedRate(() -> doSelectMaster(host, port, contextPath),
                0,
                AdminConstants.TEN_SECONDS_MILLIS_TIME,
                TimeUnit.MILLISECONDS);
    }
    
    private void doSelectMaster(final String host, final String port, final String contextPath) {
        // expires all locks older than 10 seconds
        jdbcLockRegistry.expireUnusedOlderThan(AdminConstants.TEN_SECONDS_MILLIS_TIME);
        
        // DEFAULT_TTL = 10000 ms
        Lock lock = jdbcLockRegistry.obtain(MASTER_LOCK_KEY);
        try {
            
            boolean locked = lock.tryLock(AdminConstants.TEN_SECONDS_MILLIS_TIME,
                    TimeUnit.MILLISECONDS);
            
            if (!locked) {
                LOG.info("select master fail, wait for next time");
                clusterMasterService.removeMaster();
                return;
            }
            LOG.info("select master success");
            
            // set master info (db and local flag)
            clusterMasterService.setMaster(host, port, contextPath);
            
            // upstream check task
            upstreamCheckService.setup();
            
            // load api
            loadServiceDocEntry.loadApiDocument();
            
            while (true) {
                try {
                    // sleeps 10s then renew the lock
                    TimeUnit.MILLISECONDS.sleep(AdminConstants.FIVE_SECONDS_MILLIS_TIME);
                    jdbcLockRegistry.renewLock(MASTER_LOCK_KEY);
                    LOG.info("renew master lock success");
                } catch (Exception e) {
//                    LOG.error("renew master lock fail", e);
                    // if renew fail, remove local master flag
                    clusterMasterService.removeMaster();
                    // close the upstream check service
                    upstreamCheckService.close();
                    String message = String.format("renew master lock fail, %s", e.getMessage());
                    throw new ShenyuException(message);
                }
            }
        } catch (Exception e) {
            LOG.error("select master error", e);
        } finally {
            lock.unlock();
        }
    }
}
