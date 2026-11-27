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

import org.junit.jupiter.api.Test;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.test.context.FilteredClassLoader;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * The registration diagnostics health integration is opt-in, never fails liveness, and
 * disappears entirely when Actuator is not on the classpath (issue #8430).
 */
class RegistrationDiagnosticsHealthConfigurationTest {

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(RegistrationDiagnosticsHealthConfiguration.class));

    @Test
    void optInPropertyRegistersAnAlwaysUpContributor() {
        runner.withPropertyValues("shenyu.register.diagnostics.health-enabled=true")
                .run(context -> {
                    assertThat(context).hasSingleBean(RegistrationDiagnosticsHealthIndicator.class);
                    RegistrationDiagnosticsHealthIndicator indicator = context.getBean(RegistrationDiagnosticsHealthIndicator.class);
                    assertThat(indicator.health().getStatus().getCode()).isEqualTo("UP");
                    assertThat(indicator.health().getDetails()).containsKey(RegistrationDiagnosticsHealthIndicator.SNAPSHOT_DETAIL);
                });
    }

    @Test
    void withoutTheOptInPropertyNoContributorIsRegistered() {
        runner.run(context -> assertThat(context).doesNotHaveBean(RegistrationDiagnosticsHealthIndicator.class));
    }

    @Test
    void missingActuatorDoesNotBreakTheApplicationContext() {
        runner.withClassLoader(new FilteredClassLoader(HealthIndicator.class))
                .withPropertyValues("shenyu.register.diagnostics.health-enabled=true")
                .run(context -> {
                    assertThat(context).doesNotHaveBean(RegistrationDiagnosticsHealthIndicator.class);
                    assertThat(context).hasNotFailed();
                });
    }
}
