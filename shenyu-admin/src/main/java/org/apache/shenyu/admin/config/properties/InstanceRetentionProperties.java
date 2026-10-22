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
import org.springframework.context.annotation.Configuration;

import java.time.Duration;

/**
 * Instance retention properties, used to bound the records kept for expired instances.
 */
@Configuration
@ConfigurationProperties(prefix = "shenyu.instance-retention")
public class InstanceRetentionProperties {

    /**
     * Whether to enable the cleanup of expired instance records.
     */
    private boolean enabled;

    /**
     * How long an instance in the {@code DELETED} state may stay before it is removed from memory and database.
     */
    private Duration ttl = Duration.ofDays(7);

    /**
     * Whether the retention cleanup is enabled.
     *
     * @return whether enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * Set whether the retention cleanup is enabled.
     *
     * @param enabled whether enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Get the retention duration for expired instances.
     *
     * @return retention duration
     */
    public Duration getTtl() {
        return ttl;
    }

    /**
     * Set the retention duration for expired instances.
     *
     * @param ttl retention duration
     */
    public void setTtl(final Duration ttl) {
        this.ttl = ttl;
    }
}
