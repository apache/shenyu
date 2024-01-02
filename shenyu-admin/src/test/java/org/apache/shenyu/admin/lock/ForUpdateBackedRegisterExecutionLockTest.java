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

package org.apache.shenyu.admin.lock;

import org.apache.shenyu.admin.lock.impl.ForUpdateBackedRegisterExecutionLock;
import org.apache.shenyu.admin.lock.util.RegisterTransactionUtil;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.DefaultTransactionDefinition;
import org.springframework.transaction.support.DefaultTransactionStatus;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.any;

@ExtendWith(MockitoExtension.class)
public class ForUpdateBackedRegisterExecutionLockTest {

    private ForUpdateBackedRegisterExecutionLock lock;

    @Mock
    private PlatformTransactionManager transactionManager;

    @Mock
    private PluginMapper pluginMapper;

    @BeforeEach
    public void setup() {
        lock = new ForUpdateBackedRegisterExecutionLock(transactionManager, pluginMapper, "testPlugin");
    }

    @Test
    public void lock() {
        doReturn(mock(TransactionStatus.class)).when(transactionManager).getTransaction(any(DefaultTransactionDefinition.class));
        doReturn(null).when(pluginMapper).selectByNameForUpdate("testPlugin");
        lock.lock();
        verify(transactionManager, times(1)).getTransaction(any(DefaultTransactionDefinition.class));
        verify(pluginMapper, times(1)).selectByNameForUpdate("testPlugin");
    }

    @Test
    public void unlock() {
        DefaultTransactionStatus status = new DefaultTransactionStatus(null, true, true, false, false, null);
        RegisterTransactionUtil.set(status);
        doNothing().when(transactionManager).commit(any(TransactionStatus.class));
        lock.unlock();
        verify(transactionManager, times(1)).commit(any(TransactionStatus.class));
    }
}
