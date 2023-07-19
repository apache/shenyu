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

package org.apache.shenyu.e2e.engine.service;

import org.testcontainers.containers.wait.strategy.HttpWaitStrategy;
import org.testcontainers.containers.wait.strategy.WaitStrategy;

import java.time.Duration;

/**
 * Perform health checks on Admin and Gateway.
 */
public class WaitingForStrategies {

    private static final String GET = "GET";

    private static final String ACTUATOR_HEALTH = "/actuator/health";
    
    /**
     * new a admin http wait strategy.
     * @param port port
     * @return WaitStrategy
     */
    public static WaitStrategy newAdminStrategy(final int port) {
        return new HttpWaitStrategy()
                .allowInsecure()
                .forPort(port)
                .withMethod(GET)
                .forPath(ACTUATOR_HEALTH)
                .forStatusCode(200)
                .withReadTimeout(Duration.ofSeconds(3))
                .withStartupTimeout(Duration.ofMinutes(3));
    }
    
    /**
     * new a gateway http wait strategy.
     * @param port port
     * @return WaitStrategy
     */
    public static WaitStrategy newGatewayStrategy(final int port) {
        return new HttpWaitStrategy()
                .allowInsecure()
                .forPort(port)
                .withMethod(GET)
                .forPath(ACTUATOR_HEALTH)
                .forStatusCode(200)
                .withReadTimeout(Duration.ofSeconds(3))
                .withStartupTimeout(Duration.ofMinutes(3));
    }
    
}
