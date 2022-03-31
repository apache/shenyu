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

package org.apache.shenyu.admin.listener;

import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.common.enums.DataEventTypeEnum;

import javax.annotation.Resource;

/**
 * AbstractDataChangedInit.
 */
public abstract class AbstractDataChangedInit implements DataChangedInit {

    /**
     * SyncDataService, sync all data.
     */
    @Resource
    private SyncDataService syncDataService;

    @Override
    public void run(final String... args) throws Exception {
        if (notExist()) {
            syncDataService.syncAll(DataEventTypeEnum.REFRESH);
        }
    }

    /**
     * check exist.
     *
     * @return boolean.
     */
    protected abstract boolean notExist();
}
