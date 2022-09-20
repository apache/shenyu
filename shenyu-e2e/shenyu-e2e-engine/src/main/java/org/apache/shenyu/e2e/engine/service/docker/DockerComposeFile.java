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

package org.apache.shenyu.e2e.engine.service.docker;

import lombok.Getter;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.springframework.util.ResourceUtils;
import org.testcontainers.shaded.org.yaml.snakeyaml.LoaderOptions;
import org.testcontainers.shaded.org.yaml.snakeyaml.Yaml;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class DockerComposeFile {
    @Getter
    private final List<String> services;
    
    @Getter
    private final File file;
    
    DockerComposeFile(List<String> services, File file) {
        this.services = services;
        this.file = file;
    }
    
    public static DockerComposeFile parse(String fileName) {
        final File file = Assertions.assertDoesNotThrow(
                () -> ResourceUtils.getFile(fileName),
                "docker-compose file '" + fileName + "' is not found"
        );
        final InputStream compose = Assertions.assertDoesNotThrow(
                () -> new FileInputStream(file),
                "docker-compose file '" + fileName + "' is not found"
        );
        final Yaml yaml = new Yaml(new LoaderOptions());
        Map<String, Object> content = yaml.load(compose);
        
        Object services = content.get("services");
        Assumptions.assumeTrue(services instanceof Map, "expected services was 'Map<String, ?>'");
        Map<String, Object> stringObjectMap = (Map<String, Object>) services;
        
        final List<String> result = new ArrayList<>();
        
        stringObjectMap.forEach((name, srv) -> {
            Assumptions.assumeTrue(srv instanceof Map, "expected service was 'Map<String, ?>'");
            Map<String, Object> entry = (Map<String, Object>) srv;

            // depends_on
            Object dependsOn = entry.get("depends_on");
            if (Objects.nonNull(dependsOn)) {
                if (dependsOn instanceof List) {
                    result.addAll((List<String>) dependsOn);
                } else if (dependsOn instanceof Map) {
                    ((Map<?, ?>) dependsOn).keySet().stream().map(e -> (String) e).forEach(result::add);
                } else {
                    Assertions.fail("unexpected 'depends_on' type: " + dependsOn.getClass());
                }
            }
    
            result.add(name);
        });
        return new DockerComposeFile(result, file);
    }

}
