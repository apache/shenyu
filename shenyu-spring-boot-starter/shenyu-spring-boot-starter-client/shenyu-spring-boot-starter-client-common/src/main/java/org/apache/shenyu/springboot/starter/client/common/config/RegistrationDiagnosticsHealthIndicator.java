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

import org.apache.shenyu.register.client.api.ClientRegistrationDiagnostics;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;

/**
 * Exposes the bounded registration diagnostics snapshot under the health endpoint's
 * details. Always UP: an exhausted or retrying registration is an observability fact for
 * operators to read, not a liveness verdict.
 */
public class RegistrationDiagnosticsHealthIndicator implements HealthIndicator {

    /**
     * The detail key carrying the registration diagnostics snapshot.
     */
    public static final String SNAPSHOT_DETAIL = "registration";

    @Override
    public Health health() {
        ClientRegistrationDiagnostics.RegistrationSnapshot snapshot =
                ClientRegistrationDiagnostics.getInstance().snapshot();
        return Health.up().withDetail(SNAPSHOT_DETAIL, snapshot).build();
    }
}
