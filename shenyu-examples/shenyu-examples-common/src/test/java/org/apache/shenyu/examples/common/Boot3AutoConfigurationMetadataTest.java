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

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * Verifies the Boot 3 auto-configuration registration stays in sync with the
 * legacy spring.factories entry, so the shared example configuration is loaded
 * on Spring Boot 3 (which no longer reads EnableAutoConfiguration from
 * spring.factories).
 */
public final class Boot3AutoConfigurationMetadataTest {

    @Test
    public void autoConfigurationImportsMatchSpringFactories() throws IOException {
        List<String> boot3Classes = readConfigurationClasses(Paths.get("src/main/resources/META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports"));
        List<String> boot2Classes = readConfigurationClasses(Paths.get("src/main/resources/META-INF/spring.factories")).stream()
                .filter(line -> line.startsWith("org.apache.shenyu"))
                .collect(Collectors.toList());

        assertFalse(boot2Classes.isEmpty(), "spring.factories must declare the shared configuration");
        assertEquals(boot2Classes, boot3Classes,
                "the Boot 3 AutoConfiguration.imports file must register the same configuration classes");
    }

    private List<String> readConfigurationClasses(final Path resource) throws IOException {
        assertFalse(!Files.exists(resource), resource + " is missing from the module resources");
        return Files.readAllLines(resource, StandardCharsets.UTF_8).stream()
                .map(String::trim)
                .filter(line -> !line.isEmpty() && !line.startsWith("#"))
                .collect(Collectors.toList());
    }
}
