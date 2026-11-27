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

package org.apache.shenyu.springboot.starter.plugin.messagesignature;

import org.apache.shenyu.plugin.messagesignature.MessageSignaturePlugin;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.test.context.FilteredClassLoader;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import java.security.KeyPairGenerator;
import java.security.PublicKey;

import static org.assertj.core.api.Assertions.assertThat;

class MessageSignatureAutoConfigurationTest {

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(MessageSignatureAutoConfiguration.class));

    @Test
    void verificationOnlyStartsWithNoRedisLibrary() throws Exception {
        PublicKey key = KeyPairGenerator.getInstance("Ed25519").generateKeyPair().getPublic();
        runner.withClassLoader(new FilteredClassLoader("org.springframework.data.redis"))
                .withBean("partnerKey", PublicKey.class, () -> key)
                .withPropertyValues("shenyu.message-signature.routes[0].rule-id=rule",
                        "shenyu.message-signature.routes[0].key-id=partner", "shenyu.message-signature.routes[0].public-key-bean=partnerKey")
                .run(context -> assertThat(context).hasSingleBean(MessageSignaturePlugin.class));
    }

    @Test
    void unknownVerificationKeyPreventsStartup() {
        runner.withPropertyValues("shenyu.message-signature.routes[0].rule-id=rule",
                "shenyu.message-signature.routes[0].key-id=partner", "shenyu.message-signature.routes[0].public-key-bean=absent")
                .run(context -> assertThat(context).hasFailed());
    }
}
