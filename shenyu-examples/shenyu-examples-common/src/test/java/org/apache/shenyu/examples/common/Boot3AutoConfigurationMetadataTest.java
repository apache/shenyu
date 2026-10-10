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

package org.apache.shenyu.examples.common;

import org.apache.shenyu.examples.common.aop.InterceptorConfiguration;
import org.apache.shenyu.examples.common.aop.LogInterceptor;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Verifies the shared example configuration is registered in the Boot 3
 * auto-configuration metadata (Boot 3.3 no longer reads
 * EnableAutoConfiguration entries from spring.factories) and that the
 * registration actually loads.
 */
public final class Boot3AutoConfigurationMetadataTest {

    private final ApplicationContextRunner contextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(InterceptorConfiguration.class));

    @Test
    public void configurationRegistersItsBeans() {
        contextRunner.run(context -> {
            assertNotNull(context.getBean(LogInterceptor.class), "InterceptorConfiguration must register logInterceptor");
        });
    }

    @Test
    public void autoConfigurationImportsMatchSpringFactories() throws IOException {
        List<String> boot3Classes = readConfigurationClasses("/META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports");
        List<String> boot2Classes = readConfigurationClasses("/META-INF/spring.factories").stream()
                .filter(line -> line.startsWith("org.apache.shenyu"))
                .collect(Collectors.toList());

        assertFalse(boot2Classes.isEmpty(), "spring.factories must declare the shared configuration");
        assertEquals(boot2Classes, boot3Classes,
                "the Boot 3 AutoConfiguration.imports file must register the same configuration classes");
    }

    private List<String> readConfigurationClasses(final String resource) throws IOException {
        InputStream stream = getClass().getResourceAsStream(resource);
        assertNotNull(stream, resource + " is missing from the module resources");
        List<String> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(stream, StandardCharsets.UTF_8))) {
            String line = reader.readLine();
            while (Objects.nonNull(line)) {
                lines.add(line);
                line = reader.readLine();
            }
        }
        return lines.stream()
                .map(String::trim)
                .filter(line -> !line.isEmpty() && !line.startsWith("#"))
                .collect(Collectors.toList());
    }
}
