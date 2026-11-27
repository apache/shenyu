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
import org.apache.shenyu.plugin.messagesignature.MessageSignatureVerifier;
import org.apache.shenyu.plugin.messagesignature.NonceStore;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.util.Assert;

import java.security.PublicKey;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/** Register the optional message signature plugin without requiring a Redis driver. */
@AutoConfiguration
@EnableConfigurationProperties(MessageSignatureProperties.class)
public class MessageSignatureAutoConfiguration {

    /**
     * Build route policies from trusted gateway beans.
     * @param properties deployment configuration
     * @param beans local key beans
     * @param stores optional shared nonce store
     * @return the signature plugin
     */
    @Bean
    @ConditionalOnMissingBean
    public MessageSignaturePlugin messageSignaturePlugin(final MessageSignatureProperties properties, final BeanFactory beans, final ObjectProvider<NonceStore> stores) {
        List<MessageSignatureProperties.Route> routes = Objects.requireNonNullElse(properties.routes(), List.of());
        Map<String, MessageSignatureVerifier> rules = routes.stream().collect(Collectors.toUnmodifiableMap(MessageSignatureProperties.Route::ruleId,
                route -> verifier(route, beans, stores)));
        return new MessageSignaturePlugin(rules);
    }

    private static MessageSignatureVerifier verifier(final MessageSignatureProperties.Route route, final BeanFactory beans, final ObjectProvider<NonceStore> stores) {
        Assert.hasText(route.ruleId(), "rule-id is required");
        Assert.hasText(route.keyId(), "key-id is required");
        Assert.hasText(route.publicKeyBean(), "public-key-bean is required");
        PublicKey key = beans.getBean(route.publicKeyBean(), PublicKey.class);
        return route.replayProtection() ? MessageSignatureVerifier.replayProtected(route.keyId(), key, stores.getIfAvailable())
                : MessageSignatureVerifier.verificationOnly(route.keyId(), key);
    }
}
