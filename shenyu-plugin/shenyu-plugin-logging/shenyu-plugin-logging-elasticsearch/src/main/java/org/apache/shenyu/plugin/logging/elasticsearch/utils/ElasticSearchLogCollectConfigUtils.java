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

package org.apache.shenyu.plugin.logging.elasticsearch.utils;

import org.apache.shenyu.plugin.logging.elasticsearch.config.LogCollectConfig;

import java.util.Optional;

/**
 * ElasticSearchLogCollectConfig utils.
 */
public class ElasticSearchLogCollectConfigUtils {


    private static LogCollectConfig.GlobalLogConfig globalLogConfig;

    private static final LogCollectConfig.GlobalLogConfig DEFAULT_GLOBAL_LOG_CONFIG =
            new LogCollectConfig.GlobalLogConfig();

    public ElasticSearchLogCollectConfigUtils() {
    }

    /**
     * set global config.
     *
     * @param config global config
     */
    public static void setGlobalConfig(final LogCollectConfig.GlobalLogConfig config) {
        globalLogConfig = config;
    }

    /**
     * get global config.
     *
     * @return {@linkplain LogCollectConfig.GlobalLogConfig}
     */
    public static LogCollectConfig.GlobalLogConfig getGlobalLogConfig() {
        return Optional.ofNullable(globalLogConfig).orElse(DEFAULT_GLOBAL_LOG_CONFIG);
    }
}
