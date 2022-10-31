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

package org.apache.shenyu.springboot.starter.plugin.oauth2;

import org.springframework.boot.autoconfigure.condition.ConditionOutcome;
import org.springframework.boot.autoconfigure.condition.SpringBootCondition;
import org.springframework.boot.autoconfigure.security.oauth2.client.OAuth2ClientProperties;
import org.springframework.boot.context.properties.bind.Bindable;
import org.springframework.boot.context.properties.bind.Binder;
import org.springframework.context.annotation.ConditionContext;
import org.springframework.core.env.Environment;
import org.springframework.core.type.AnnotatedTypeMetadata;

import java.util.Collections;
import java.util.Map;

/**
 * The OAuth2 plugin default repository config condition.
 * if not spring oauth2's config in .yml file, use default java config, else use spring config.
 *
 * @see org.springframework.boot.autoconfigure.security.oauth2.client.ClientsConfiguredCondition
 */
public class DefaultClientsConfiguredCondition extends SpringBootCondition {
    private static final Bindable<Map<String, OAuth2ClientProperties.Registration>> STRING_REGISTRATION_MAP = Bindable.mapOf(String.class, OAuth2ClientProperties.Registration.class);

    @Override
    public ConditionOutcome getMatchOutcome(final ConditionContext context, final AnnotatedTypeMetadata metadata) {
        Map<String, OAuth2ClientProperties.Registration> registrations = this.getRegistrations(context.getEnvironment());
        return registrations.isEmpty() ? ConditionOutcome.match("use default configuration") : ConditionOutcome.noMatch("use user configuration");
    }

    private Map<String, OAuth2ClientProperties.Registration> getRegistrations(final Environment environment) {
        return Binder.get(environment).bind("spring.security.oauth2.client.registration", STRING_REGISTRATION_MAP).orElse(Collections.emptyMap());
    }
}
