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

package org.apache.shenyu.springboot.starter.plugin.logging.tencent.cls;

import org.apache.shenyu.plugin.tencent.cls.LoggingTencentClsPlugin;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.tencent.cls.handler.LoggingTencentClsPluginDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Logging Tencent cls plugin configuration.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.logging-tencent-cls.enabled"}, havingValue = "true", matchIfMissing = true)
public class LoggingTencentClsPluginConfiguration {

    /**
     * logging Tencent cls plugin data handler.
     * @return logging Tencent cls PluginDataHandler
     */
    @Bean
    public PluginDataHandler loggingTencentClsPluginDataHandler() {
        return new LoggingTencentClsPluginDataHandler();
    }

    /**
     * Logging Tencent cls plugin.
     * @return LoggingTencentClsPlugin
     */
    @Bean
    public ShenyuPlugin loggingTencentClsPlugin() {
        return new LoggingTencentClsPlugin();
    }
}
