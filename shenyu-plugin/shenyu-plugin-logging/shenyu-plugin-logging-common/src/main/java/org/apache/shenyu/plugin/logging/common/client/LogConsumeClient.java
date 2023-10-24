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

package org.apache.shenyu.plugin.logging.common.client;

import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;

import java.util.List;

/**
 * Used to collect logs, which can be stored in remote or local files or databases, or others.
 */
public interface LogConsumeClient<C extends GenericGlobalConfig, L extends ShenyuRequestLog> extends AutoCloseable {
    
    /**
     * collect logs.
     *
     * @param logs list of log
     * @throws Exception produce exception
     */
    void consume(List<L> logs) throws Exception;

    /**
     * init client by config.
     * @param config logClientConfig
     */
    void initClient(C config);
}
