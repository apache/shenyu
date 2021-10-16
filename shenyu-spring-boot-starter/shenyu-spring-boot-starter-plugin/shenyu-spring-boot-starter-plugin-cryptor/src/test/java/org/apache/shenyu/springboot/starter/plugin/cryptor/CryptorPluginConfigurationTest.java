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

package org.apache.shenyu.springboot.starter.plugin.cryptor;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.cryptor.request.CryptorRequestPlugin;
import org.apache.shenyu.plugin.cryptor.response.CryptorResponsePlugin;
import org.junit.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Test case for {@link CryptorPluginConfiguration}.
 */
public class CryptorPluginConfigurationTest {

    @Test
    public void testCryptorRequestPlugin() {
        new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(CryptorPluginConfiguration.class))
                .run(context -> {
                    CryptorRequestPlugin plugin = context.getBean("cryptorRequestPlugin", CryptorRequestPlugin.class);
                    assertThat(plugin.named()).isEqualTo(PluginEnum.CRYPTOR_REQUEST.getName());
                });
    }

    @Test
    public void testCryptorResponsePlugin() {
        new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(CryptorPluginConfiguration.class))
                .run(context -> {
                    CryptorResponsePlugin plugin = context.getBean("cryptorResponsePlugin", CryptorResponsePlugin.class);
                    assertThat(plugin.named()).isEqualTo(PluginEnum.CRYPTOR_RESPONSE.getName());
                });
    }
}
