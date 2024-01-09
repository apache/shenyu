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
import org.apache.shenyu.admin.lock.RegisterExecutionRepository;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.springframework.transaction.PlatformTransactionManager;

public class PlatformTransactionRegisterExecutionRepository implements RegisterExecutionRepository {

    private final PlatformTransactionManager platformTransactionManager;

    private final PluginMapper pluginMapper;

    public PlatformTransactionRegisterExecutionRepository(final PlatformTransactionManager platformTransactionManager, final PluginMapper pluginMapper) {
        this.platformTransactionManager = platformTransactionManager;
        this.pluginMapper = pluginMapper;
    }

    @Override
    public RegisterExecutionLock getLock(final String key) {
        return new ForUpdateBackedRegisterExecutionLock(platformTransactionManager, pluginMapper, key);
    }
}
