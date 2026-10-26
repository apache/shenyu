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

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * Port policy for Swagger import, MCP import and connection testing.
 * This policy never permits private or internal destination addresses.
 */
@ConfigurationProperties(prefix = "shenyu.swagger")
public class SwaggerImportProperties {

    private Set<Integer> allowedPorts = Collections.emptySet();

    private Set<Integer> deniedPorts = Set.of(22, 23, 3389, 25, 53, 135, 139, 445,
            5432, 3306, 1433, 6379, 11211, 5984, 9200, 2181, 9092, 9093,
            2375, 2376, 25672, 5672, 15672, 4369);

    /**
     * Get allowed ports. An empty set permits any port not denied.
     * @return allowed ports
     */
    public Set<Integer> getAllowedPorts() {
        return allowedPorts;
    }

    /**
     * Set allowed ports. Denied ports take precedence.
     * @param allowedPorts allowed ports
     */
    public void setAllowedPorts(final Set<Integer> allowedPorts) {
        this.allowedPorts = validatedPorts(allowedPorts);
    }

    /**
     * Get denied ports.
     * @return denied ports
     */
    public Set<Integer> getDeniedPorts() {
        return deniedPorts;
    }

    /**
     * Replace denied ports. An empty set disables only the port deny list.
     * @param deniedPorts denied ports
     */
    public void setDeniedPorts(final Set<Integer> deniedPorts) {
        this.deniedPorts = validatedPorts(deniedPorts);
    }

    private Set<Integer> validatedPorts(final Set<Integer> ports) {
        if (Objects.isNull(ports) || ports.stream().anyMatch(port -> Objects.isNull(port) || port < 1 || port > 65535)) {
            throw new IllegalArgumentException("Swagger ports must be between 1 and 65535");
        }
        return Collections.unmodifiableSet(new HashSet<>(ports));
    }
}
