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

package org.apache.shenyu.springboot.starter.plugin.jwe;

import org.apache.shenyu.plugin.jwe.JwePayloadPlugin;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import java.security.KeyPairGenerator;
import java.security.interfaces.RSAPrivateKey;

import static org.assertj.core.api.Assertions.assertThat;

class JwePayloadAutoConfigurationTest {

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(JwePayloadAutoConfiguration.class));

    @Test
    void bindsAnInboundKeyByNamespaceAndRuleAndAppliesLimits() throws Exception {
        RSAPrivateKey key = (RSAPrivateKey) KeyPairGenerator.getInstance("RSA").generateKeyPair().getPrivate();
        runner.withBean("mountedKey", RSAPrivateKey.class, () -> key).withPropertyValues(
                "shenyu.jwe-payload.routes[0].namespace-id=test", "shenyu.jwe-payload.routes[0].rule-id=echo",
                "shenyu.jwe-payload.routes[0].decryption-keys[Key-A]=mountedKey", "shenyu.jwe-payload.max-plaintext-bytes=2048")
                .run(context -> {
                    assertThat(context).hasSingleBean(JwePayloadPlugin.class);
                    JwePayloadProperties properties = context.getBean(JwePayloadProperties.class);
                    assertThat(properties.maxPlaintextBytes()).isEqualTo(2048);
                    assertThat(properties.routes().get(0).decryptionKeys()).containsEntry("Key-A", "mountedKey");
                });
    }

    @Test
    void missingNamedKeyOrNonPositiveLimitPreventsStartup() {
        runner.withPropertyValues("shenyu.jwe-payload.routes[0].namespace-id=test", "shenyu.jwe-payload.routes[0].rule-id=echo",
                "shenyu.jwe-payload.routes[0].decryption-keys[k1]=missingKey").run(context -> assertThat(context).hasFailed());
        runner.withPropertyValues("shenyu.jwe-payload.max-ciphertext-bytes=0").run(context -> assertThat(context).hasFailed());
        runner.withPropertyValues("spring.codec.max-in-memory-size=0B").run(context -> assertThat(context).hasFailed());
    }
}
