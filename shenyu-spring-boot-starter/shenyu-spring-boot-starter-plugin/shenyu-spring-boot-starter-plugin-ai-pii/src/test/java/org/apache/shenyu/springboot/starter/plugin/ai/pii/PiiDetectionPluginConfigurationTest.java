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

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.pii.audit.LoggingPiiAuditSink;
import org.apache.shenyu.plugin.ai.pii.audit.PiiAuditSink;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.codec.support.DefaultServerCodecConfigurer;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for {@link PiiDetectionPluginConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class PiiDetectionPluginConfigurationTest {

    @Test
    public void testPiiDetectionPlugin() {
        new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(PiiDetectionPluginConfiguration.class, DefaultServerCodecConfigurer.class))
                .withBean(PiiDetectionPluginConfigurationTest.class)
                .withPropertyValues("debug=true")
                .run(context -> {
                    ShenyuPlugin plugin = context.getBean("piiDetectionPlugin", ShenyuPlugin.class);
                    assertNotNull(plugin);
                    assertThat(plugin.named()).isEqualTo(PluginEnum.PII_DETECTION.getName());
                });
    }

    @Test
    public void testPiiDetectionPluginDataHandler() {
        new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(PiiDetectionPluginConfiguration.class, DefaultServerCodecConfigurer.class))
                .withBean(PiiDetectionPluginConfigurationTest.class)
                .withPropertyValues("debug=true")
                .run(context -> {
                    PluginDataHandler handler = context.getBean("piiDetectionPluginDataHandler", PluginDataHandler.class);
                    assertNotNull(handler);
                    assertThat(handler.pluginNamed()).isEqualTo(PluginEnum.PII_DETECTION.getName());
                });
    }

    @Test
    public void testDefaultAuditSink() {
        new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(PiiDetectionPluginConfiguration.class, DefaultServerCodecConfigurer.class))
                .withBean(PiiDetectionPluginConfigurationTest.class)
                .withPropertyValues("debug=true")
                .run(context -> {
                    PiiAuditSink sink = context.getBean("piiAuditSink", PiiAuditSink.class);
                    assertNotNull(sink);
                    assertThat(sink).isInstanceOf(LoggingPiiAuditSink.class);
                });
    }

    @Test
    public void testAutoConfigurationImportsResourceListsTheConfiguration() throws Exception {
        // a starter without this resource is silently never assembled in the
        // bootstrap, the configuration class alone is not enough
        String resource = "META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports";
        try (java.io.InputStream stream = getClass().getClassLoader().getResourceAsStream(resource)) {
            assertNotNull(stream, resource + " is missing from the starter");
            String content = new String(java.util.Objects.requireNonNull(stream).readAllBytes(),
                    java.nio.charset.StandardCharsets.UTF_8);
            assertThat(content).contains(PiiDetectionPluginConfiguration.class.getName());
        }
    }
}
