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

package org.apache.shenyu.springboot.starter.plugin.ai.requestid;

import org.apache.shenyu.plugin.ai.requestid.AiRequestIdPlugin;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiRequestIdPluginConfigurationTest {

    private final ApplicationContextRunner contextRunner = new ApplicationContextRunner()
            .withUserConfiguration(AiRequestIdPluginConfiguration.class);

    @Test
    void shouldAssemblePluginAndHandlerByDefault() {
        contextRunner.run(context -> {
            assertTrue(context.getBean("aiRequestIdPlugin") instanceof AiRequestIdPlugin);
            assertTrue(context.getBean(ShenyuPlugin.class) instanceof AiRequestIdPlugin);
            assertTrue(context.getBean("aiRequestIdPluginDataHandler") instanceof PluginDataHandler);
        });
    }

    @Test
    void shouldNotAssembleWhenExplicitlyDisabled() {
        contextRunner.withPropertyValues("shenyu.plugins.ai.request.id.enabled=false")
                .run(context -> {
                    assertFalse(context.containsBean("aiRequestIdPlugin"));
                    assertFalse(context.containsBean("aiRequestIdPluginDataHandler"));
                });
    }
}
