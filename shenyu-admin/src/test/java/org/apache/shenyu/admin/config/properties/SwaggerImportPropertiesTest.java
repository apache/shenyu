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

package org.apache.shenyu.admin.config.properties;

import org.apache.shenyu.admin.config.HttpUtilsConfiguration;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test Swagger import port policy binding.
 */
public class SwaggerImportPropertiesTest {

    private final ApplicationContextRunner contextRunner = new ApplicationContextRunner().withUserConfiguration(HttpUtilsConfiguration.class);

    @Test
    public void shouldBindAllowedAndDeniedPorts() {
        contextRunner.withPropertyValues("shenyu.swagger.allowed-ports=80,443,8080", "shenyu.swagger.denied-ports=8080,9090")
                .run(context -> {
                    SwaggerImportProperties properties = context.getBean(SwaggerImportProperties.class);
                    assertEquals(Set.of(80, 443, 8080), properties.getAllowedPorts());
                    assertEquals(Set.of(8080, 9090), properties.getDeniedPorts());
                });
    }

    @Test
    public void shouldAllowExplicitEmptyDenyList() {
        contextRunner.withPropertyValues("shenyu.swagger.denied-ports=")
                .run(context -> assertTrue(context.getBean(SwaggerImportProperties.class).getDeniedPorts().isEmpty()));
    }

    @Test
    public void shouldRejectInvalidBoundPorts() {
        contextRunner.withPropertyValues("shenyu.swagger.allowed-ports=65536")
                .run(context -> assertNotNull(context.getStartupFailure()));
        contextRunner.withPropertyValues("shenyu.swagger.denied-ports=0")
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    public void shouldCopyConfigurationAndRejectInvalidValues() {
        SwaggerImportProperties properties = new SwaggerImportProperties();
        Set<Integer> ports = new HashSet<>(Set.of(8080));
        properties.setAllowedPorts(ports);
        ports.clear();
        assertEquals(Set.of(8080), properties.getAllowedPorts());
        assertThrows(UnsupportedOperationException.class, () -> properties.getAllowedPorts().clear());
        assertThrows(IllegalArgumentException.class, () -> properties.setDeniedPorts(null));
        assertThrows(IllegalArgumentException.class, () -> properties.setDeniedPorts(Collections.singleton(null)));
        assertThrows(IllegalArgumentException.class, () -> properties.setAllowedPorts(Set.of(-1)));
    }
}
