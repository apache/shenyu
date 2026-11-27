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

package org.apache.shenyu.springboot.starter.client.common.config;

import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Opt-in Spring Boot health integration for the client registration diagnostics.
 *
 * <p>Active only when Spring Boot Actuator is on the classpath and the operator sets
 * {@code shenyu.register.diagnostics.health-enabled=true} (default {@code false}). The
 * contributor always reports UP with the bounded snapshot in its details, so this never
 * fails application liveness or readiness by default: registration diagnostics are
 * observability, not a process-aliveness signal. The property is documented for tooling
 * in {@code META-INF/additional-spring-configuration-metadata.json}.</p>
 */
@Configuration(proxyBeanMethods = false)
@ConditionalOnClass(name = "org.springframework.boot.actuate.health.HealthIndicator")
@ConditionalOnProperty(value = "shenyu.register.diagnostics.health-enabled", havingValue = "true")
public class RegistrationDiagnosticsHealthConfiguration {

    /**
     * Health contributor bean exposing the registration diagnostics snapshot.
     *
     * @return the registration diagnostics health indicator
     */
    @Bean
    public RegistrationDiagnosticsHealthIndicator registrationDiagnosticsHealthIndicator() {
        return new RegistrationDiagnosticsHealthIndicator();
    }
}
