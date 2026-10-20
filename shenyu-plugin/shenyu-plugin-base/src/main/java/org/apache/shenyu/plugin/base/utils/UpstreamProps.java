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

package org.apache.shenyu.plugin.base.utils;

import org.apache.shenyu.common.utils.GsonUtils;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * Typed view over the raw props payload of a discovery upstream.
 *
 * <p>Centralizes the property names and defaults shared by the discovery upstream handlers,
 * so that warmup, gray and health check flags cannot drift between plugins.
 */
public final class UpstreamProps {

    private static final String WARMUP = "warmup";

    private static final String GRAY = "gray";

    private static final String HEALTH_CHECK_ENABLED = "healthCheckEnabled";

    private static final String DEFAULT_WARMUP = "10";

    private static final String DEFAULT_GRAY = "false";

    private static final String DEFAULT_HEALTH_CHECK_ENABLED = "true";

    private final Map<String, String> props;

    private UpstreamProps(final Map<String, String> props) {
        this.props = props;
    }

    /**
     * Parse the raw props payload of a discovery upstream.
     *
     * @param props the raw props json, may be null
     * @return the parsed props
     */
    public static UpstreamProps parse(final String props) {
        return new UpstreamProps(Optional.ofNullable(props)
                .map(ps -> GsonUtils.getInstance().toObjectMap(ps, String.class))
                .orElseGet(HashMap::new));
    }

    /**
     * Get the warmup, {@code 10} by default.
     *
     * @return the warmup
     */
    public int getWarmup() {
        return Integer.parseInt(getProperty(WARMUP, DEFAULT_WARMUP));
    }

    /**
     * Whether the upstream is a gray upstream, {@code false} by default.
     *
     * @return true if the upstream is gray
     */
    public boolean isGray() {
        return Boolean.parseBoolean(getProperty(GRAY, DEFAULT_GRAY));
    }

    /**
     * Whether the health check is enabled, {@code true} by default.
     *
     * @return true if the health check is enabled
     */
    public boolean isHealthCheckEnabled() {
        return Boolean.parseBoolean(getProperty(HEALTH_CHECK_ENABLED, DEFAULT_HEALTH_CHECK_ENABLED));
    }

    /**
     * Get all parsed props as a mutable map.
     *
     * @return a copy of the parsed props
     */
    public Map<String, String> toMap() {
        return new HashMap<>(props);
    }

    private String getProperty(final String key, final String defaultValue) {
        return Optional.ofNullable(props.get(key)).orElse(defaultValue);
    }
}
