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

package org.apache.shenyu.k8s.common;

import org.apache.commons.lang3.StringUtils;

import java.util.Map;
import java.util.Objects;

/**
 * Utilities for ingress configuration.
 */
public final class IngressUtils {

    private IngressUtils() {
    }

    /**
     * Get the configured protocol for an upstream.
     *
     * @param annotations ingress annotations
     * @param index upstream index
     * @param defaultProtocol default protocol
     * @return configured protocol or the default protocol
     */
    public static String getUpstreamProtocol(final Map<String, String> annotations, final int index, final String defaultProtocol) {
        if (Objects.isNull(annotations)) {
            return defaultProtocol;
        }
        String configuredProtocols = annotations.get(IngressConstants.UPSTREAMS_PROTOCOL_ANNOTATION_KEY);
        if (StringUtils.isBlank(configuredProtocols)) {
            return defaultProtocol;
        }
        String[] protocols = configuredProtocols.split(",");
        if (index >= protocols.length || StringUtils.isBlank(protocols[index])) {
            return defaultProtocol;
        }
        return protocols[index].trim();
    }
}
