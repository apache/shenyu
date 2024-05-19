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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.integration.jdbc.lock.JdbcLockRegistry;

import java.time.Duration;
import java.util.concurrent.locks.Lock;

public class ShenyuClusterSelectMasterJdbcService implements ShenyuClusterSelectMasterService {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuClusterSelectMasterJdbcService.class);
    
    private static final String MASTER_LOCK_KEY = "shenyu:cluster:master";
    
    private final JdbcLockRegistry jdbcLockRegistry;
    
    private final Lock clusterMasterLock;
    
    private volatile boolean locked;
    
    public ShenyuClusterSelectMasterJdbcService(final JdbcLockRegistry jdbcLockRegistry) {
        this.jdbcLockRegistry = jdbcLockRegistry;
        // the lock duration 15s
        this.jdbcLockRegistry.setIdleBetweenTries(Duration.ofMillis(15000));
        this.clusterMasterLock = jdbcLockRegistry.obtain(MASTER_LOCK_KEY);
    }
    
    @Override
    public boolean selectMaster() {
        locked = clusterMasterLock.tryLock();
        return locked;
    }
    
    @Override
    public boolean renewMaster() {
        if (locked) {
            try {
                jdbcLockRegistry.renewLock(MASTER_LOCK_KEY);
            } catch (Exception e) {
                LOG.error("jdbc renew master error", e);
                if (locked) {
                    locked = false;
                    clusterMasterLock.unlock();
                }
            }
        }
        return locked;
    }
    
    @Override
    public boolean releaseMaster() {
        if (locked) {
            clusterMasterLock.unlock();
        }
        return true;
    }
}
