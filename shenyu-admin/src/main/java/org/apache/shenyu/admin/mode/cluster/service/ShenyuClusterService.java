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

package org.apache.shenyu.admin.mode.cluster.service;

import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mode.ShenyuRunningModeService;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.service.manager.LoadServiceDocEntry;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class ShenyuClusterService implements ShenyuRunningModeService {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuClusterService.class);
    
    private final ClusterSelectMasterService shenyuClusterSelectMasterService;
    
    private final UpstreamCheckService upstreamCheckService;
    
    private final LoadServiceDocEntry loadServiceDocEntry;
    
    private final ScheduledExecutorService executorService;
    
    private final ClusterProperties clusterProperties;
    
    public ShenyuClusterService(final ClusterSelectMasterService shenyuClusterSelectMasterService,
                                final UpstreamCheckService upstreamCheckService,
                                final LoadServiceDocEntry loadServiceDocEntry,
                                final ClusterProperties clusterProperties) {
        this.shenyuClusterSelectMasterService = shenyuClusterSelectMasterService;
        this.upstreamCheckService = upstreamCheckService;
        this.loadServiceDocEntry = loadServiceDocEntry;
        this.clusterProperties = clusterProperties;
        this.executorService = new ScheduledThreadPoolExecutor(1,
                ShenyuThreadFactory.create("master-selector", true));
    }
    
    /**
     * start master select task.
     *
     * @param host host
     * @param port port
     * @param contextPath contextPath
     */
    public void startSelectMasterTask(final String host, final String port, final String contextPath) {
        LOG.info("starting select master task");
        // schedule task selectPeriod seconds
        executorService.scheduleAtFixedRate(() -> doSelectMaster(host, port, contextPath),
                0,
                clusterProperties.getSelectPeriod(),
                TimeUnit.SECONDS);
    }
    
    private void doSelectMaster(final String host, final String port, final String contextPath) {
        // try getting lock
        try {
            boolean selected = shenyuClusterSelectMasterService.selectMaster(host, port, contextPath);
            if (!selected) {
                LOG.info("select master fail, wait for next period");
                return;
            }
            
            LOG.info("select master success");
            
            // start upstream check task
            upstreamCheckService.setup();
            
            // load api
            loadServiceDocEntry.loadApiDocument();
            
            boolean renewed = shenyuClusterSelectMasterService.checkMasterStatus();
            
            while (renewed) {
                // sleeps selectPeriod seconds then renew the lock
                TimeUnit.SECONDS.sleep(clusterProperties.getSelectPeriod());
                
                renewed = shenyuClusterSelectMasterService.checkMasterStatus();
                if (renewed) {
                    if (LOG.isDebugEnabled()) {
                        LOG.debug("renew master success");
                    }
                }
            }
        } catch (Exception e) {
            LOG.error("select master error", e);
            // close the upstream check service
            upstreamCheckService.close();
            
            String message = String.format("renew master fail, %s", e.getMessage());
            throw new ShenyuException(message);
        } finally {
            try {
                shenyuClusterSelectMasterService.releaseMaster();
            } catch (Exception e) {
                LOG.error("release master error", e);
            }
        }
    }
    
    @Override
    public void start(final String host, final int port, final String contextPath) {
        startSelectMasterTask(host, String.valueOf(port), contextPath);
    }
    
    @Override
    public void shutdown() {
    
    }
}
