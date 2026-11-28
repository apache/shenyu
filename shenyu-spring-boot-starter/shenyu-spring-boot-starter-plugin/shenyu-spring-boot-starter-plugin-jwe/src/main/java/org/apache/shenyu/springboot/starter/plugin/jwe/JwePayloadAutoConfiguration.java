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
import org.apache.shenyu.plugin.jwe.JwePayloadPlugin.RuleKey;
import org.apache.shenyu.plugin.jwe.JweRoute;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.context.properties.bind.Binder;
import org.springframework.context.annotation.Bean;
import org.springframework.core.env.Environment;
import org.springframework.util.unit.DataSize;

import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/** Resolve the configured key beans once, before accepting any encrypted requests. */
@AutoConfiguration
@EnableConfigurationProperties(JwePayloadProperties.class)
public class JwePayloadAutoConfiguration {

    /**
     * Create the JWE plugin using deployment-owned key rings and payload limits.
     * @param properties bound gateway policy
     * @param beans local key beans
     * @param environment gateway codec settings
     * @return JWE plugin
     */
    @Bean
    @ConditionalOnMissingBean
    public JwePayloadPlugin jwePayloadPlugin(final JwePayloadProperties properties, final BeanFactory beans,
                                             final Environment environment) {
        List<JwePayloadProperties.Route> routes = Objects.requireNonNullElse(properties.routes(), List.of());
        Map<RuleKey, JweRoute> policies = routes.stream().collect(Collectors.toUnmodifiableMap(route -> new RuleKey(route.namespaceId(), route.ruleId()),
                route -> keys(route, beans)));
        Binder binder = Binder.get(environment);
        DataSize codecLimit = binder.bind("spring.http.codecs.max-in-memory-size", DataSize.class)
                .orElseGet(() -> binder.bind("spring.codec.max-in-memory-size", DataSize.class).orElse(DataSize.ofBytes(-1)));
        return new JwePayloadPlugin(policies, bounded(properties.maxCiphertextBytes(), codecLimit), bounded(properties.maxPlaintextBytes(), codecLimit));
    }

    private static int bounded(final int requested, final DataSize codecLimit) {
        long ceiling = codecLimit.toBytes();
        return ceiling < 0 ? requested : (int) Math.min(ceiling, requested);
    }

    private static JweRoute keys(final JwePayloadProperties.Route route, final BeanFactory beans) {
        Map<String, String> names = Objects.requireNonNullElse(route.decryptionKeys(), Map.of());
        Map<String, RSAPrivateKey> inbound = names.entrySet().stream().collect(Collectors.toUnmodifiableMap(Map.Entry::getKey,
                key -> beans.getBean(key.getValue(), RSAPrivateKey.class)));
        RSAPublicKey outbound = Objects.isNull(route.recipientKeyBean()) ? null : beans.getBean(route.recipientKeyBean(), RSAPublicKey.class);
        return new JweRoute(inbound, route.recipientKeyId(), outbound);
    }
}
