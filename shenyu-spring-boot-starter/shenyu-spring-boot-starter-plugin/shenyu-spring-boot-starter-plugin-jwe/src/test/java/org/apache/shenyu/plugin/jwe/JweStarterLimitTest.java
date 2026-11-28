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

package org.apache.shenyu.plugin.jwe;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.springboot.starter.plugin.jwe.JwePayloadAutoConfiguration;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;

import java.security.KeyPairGenerator;
import java.security.interfaces.RSAPublicKey;
import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

class JweStarterLimitTest {

    @Test
    void gatewayCodecLimitCapsTheLargerPluginLimit() throws Exception {
        RSAPublicKey key = (RSAPublicKey) KeyPairGenerator.getInstance("RSA").generateKeyPair().getPublic();
        new ApplicationContextRunner().withConfiguration(AutoConfigurations.of(JwePayloadAutoConfiguration.class))
                .withBean("recipient", RSAPublicKey.class, () -> key)
                .withPropertyValues("spring.codec.max-in-memory-size=8B", "shenyu.jwe-payload.max-plaintext-bytes=1024",
                        "shenyu.jwe-payload.routes[0].namespace-id=partner", "shenyu.jwe-payload.routes[0].rule-id=echo",
                        "shenyu.jwe-payload.routes[0].recipient-key-id=active", "shenyu.jwe-payload.routes[0].recipient-key-bean=recipient")
                .run(context -> {
                    MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/echo")
                            .contentType(MediaType.APPLICATION_JSON).body("{\"value\":123}"));
                    JwePayloadPlugin plugin = context.getBean(JwePayloadPlugin.class);
                    plugin.doExecute(exchange, ignored -> fail("oversized body forwarded"), null,
                            RuleData.builder().namespaceId("partner").id("echo").build()).block(Duration.ofSeconds(5));
                    assertEquals(HttpStatus.PAYLOAD_TOO_LARGE, exchange.getResponse().getStatusCode());
                });
    }
}
