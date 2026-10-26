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

package org.apache.shenyu.springboot.starter.plugin.ai.pii;

import org.apache.shenyu.plugin.ai.pii.PiiDetectionPlugin;
import org.apache.shenyu.plugin.ai.pii.audit.LoggingPiiAuditSink;
import org.apache.shenyu.plugin.ai.pii.audit.PiiAuditSink;
import org.apache.shenyu.plugin.ai.pii.handler.PiiDetectionPluginDataHandler;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.codec.ServerCodecConfigurer;

/**
 * The pii detection plugin configuration.
 */
@Configuration
@ConditionalOnProperty(value = "shenyu.plugins.pii-detection.enabled", havingValue = "true", matchIfMissing = true)
public class PiiDetectionPluginConfiguration {

    /**
     * the pii detection plugin.
     *
     * @param configurer the server codec configurer
     * @param auditSink the audit sink
     * @return the pii detection plugin
     */
    @Bean
    public ShenyuPlugin piiDetectionPlugin(final ServerCodecConfigurer configurer, final PiiAuditSink auditSink) {
        return new PiiDetectionPlugin(configurer.getReaders(), auditSink);
    }

    /**
     * the pii detection plugin data handler, it caches the compiled rules.
     *
     * @return the pii detection plugin data handler
     */
    @Bean
    public PluginDataHandler piiDetectionPluginDataHandler() {
        return new PiiDetectionPluginDataHandler();
    }

    /**
     * the default audit sink, an alternative bean replaces it without touching
     * the plugin, so the records can be forwarded to the unified AI invocation
     * auditing once its event vocabulary lands.
     *
     * @return the logging audit sink
     */
    @Bean
    @ConditionalOnMissingBean(PiiAuditSink.class)
    public PiiAuditSink piiAuditSink() {
        return new LoggingPiiAuditSink();
    }
}
