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

package org.apache.shenyu.admin.lock.impl;

import org.apache.shenyu.admin.lock.RegisterExecutionLock;
import org.apache.shenyu.admin.lock.util.RegisterTransactionUtil;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.DefaultTransactionDefinition;

public class ForUpdateBackedRegisterExecutionLock implements RegisterExecutionLock {

    private final PlatformTransactionManager transactionManager;

    private final PluginMapper pluginMapper;

    private final String name;

    private int lockTimeoutSeconds = 30;

    public ForUpdateBackedRegisterExecutionLock(final PlatformTransactionManager transactionManager, final PluginMapper pluginMapper, final String name) {
        this.transactionManager = transactionManager;
        this.pluginMapper = pluginMapper;
        this.name = name;
    }

    /**
     *  select ... for update not allowing for the lock to be reentrant lock
     */
    @Override
    public void lock() {
        DefaultTransactionDefinition defaultTransactionDefinition = new DefaultTransactionDefinition();
        defaultTransactionDefinition.setTimeout(lockTimeoutSeconds);
        TransactionStatus transaction = transactionManager.getTransaction(defaultTransactionDefinition);
        RegisterTransactionUtil.set(transaction);
        pluginMapper.selectByNameForUpdate(name);
    }

    @Override
    public void unlock() {
        TransactionStatus transactionStatus = RegisterTransactionUtil.get();
        try {
            if (transactionStatus.isRollbackOnly()) {
                transactionManager.rollback(transactionStatus);
                return;
            }
            transactionManager.commit(transactionStatus);
        } finally {
            RegisterTransactionUtil.remove();
        }
    }

    /**
     * Returns the time period that can elapse before a timeout occurs on an attempt to acquire a  lock. The default is 30 seconds.
     * @return lockTimeoutSeconds
     */
    public int getLockTimeoutSeconds() {
        return lockTimeoutSeconds;
    }

    /**
     * Sets the time period that can elapse before a timeout occurs on an attempt to acquire a lock. The
     * default is 30 seconds.
     * @param lockTimeoutSeconds the timeout period in seconds
     */
    public void setLockTimeoutSeconds(final int lockTimeoutSeconds) {
        this.lockTimeoutSeconds = lockTimeoutSeconds;
    }
}
