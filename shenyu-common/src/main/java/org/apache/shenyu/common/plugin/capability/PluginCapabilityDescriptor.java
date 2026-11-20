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

package org.apache.shenyu.common.plugin.capability;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Optional implementation declaration, never proof of request selection or authorization.
 * @param schemaVersion descriptor format
 * @param version declaration version
 * @param plugin stable plugin identity
 * @param implementation exact implementation class name
 * @param contexts declared protocol/context support
 * @param modes conditional body/response behavior
 * @param provides installation-level capabilities
 * @param requires required cooperating execution capabilities
 * @param supportingRequirements supporting-provider requirements, separately observed
 * @param configurationReference optional unresolved configuration-contract reference
 * @param provenance declaration provenance
 */
public record PluginCapabilityDescriptor(int schemaVersion, String version, String plugin, String implementation,
                                         Map<String, Support> contexts, List<Mode> modes, Set<String> provides, Set<String> requires,
                                         Set<String> supportingRequirements, String configurationReference, String provenance) {

    public PluginCapabilityDescriptor {
        if (schemaVersion != 1) {
            throw new IllegalArgumentException("Unsupported plugin capability descriptor schema");
        }
        identifier(version, 32);
        identifier(plugin, 64);
        text(implementation, 256);
        contexts = Map.copyOf(contexts);
        contexts.keySet().forEach(value -> identifier(value, 64));
        modes = List.copyOf(modes);
        provides = capabilities(provides);
        requires = capabilities(requires);
        supportingRequirements = capabilities(supportingRequirements);
        text(provenance, 256);
        if (Objects.nonNull(configurationReference)) {
            text(configurationReference, 128);
        }
        if (contexts.size() > 16 || modes.size() > 8) {
            throw new IllegalArgumentException("Plugin capability descriptor limit exceeded");
        }
    }

    private static Set<String> capabilities(final Set<String> source) {
        Set<String> result = Set.copyOf(source);
        if (result.size() > 32) {
            throw new IllegalArgumentException("Plugin capability set limit exceeded");
        }
        result.forEach(value -> identifier(value, 64));
        return result;
    }

    private static void identifier(final String value, final int limit) {
        if (Objects.isNull(value) || value.length() > limit || !value.matches("[A-Za-z0-9_.:-]+")) {
            throw new IllegalArgumentException("Invalid plugin capability identifier");
        }
    }

    private static void text(final String value, final int limit) {
        if (Objects.isNull(value) || value.isBlank() || value.length() > limit || value.chars().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException("Invalid plugin capability text");
        }
    }

    /** Declared support is conditional on configuration/matching and not a runtime guarantee. */
    public enum Support {
        DECLARED, CONDITIONAL, UNKNOWN, UNSUPPORTED
    }

    /** Explicit body behaviors, with unknown preserved for unconverted implementations. */
    public enum Body {
        NONE, PASS_THROUGH, STREAM_RELAY, BOUNDED_BUFFER, BUFFERED, TRANSFORM, UNKNOWN
    }

    /** Cooperating response roles; a router is not automatically an upstream invoker or writer. */
    public enum ResponseEffect {
        DECORATE, SHORT_CIRCUIT, COMMIT, PRODUCE_RESULT, CONSUME_RESULT, NONE
    }

    /**
     * A conditional declaration, never an unconditional streaming-safe flag.
     * @param when applicable configuration/execution condition
     * @param requestBody request body behavior
     * @param responseBody response body behavior
     * @param responseEffects possible response roles in this mode
     */
    public record Mode(String when, Body requestBody, Body responseBody, Set<ResponseEffect> responseEffects) {

        public Mode {
            text(when, 512);
            Objects.requireNonNull(requestBody);
            Objects.requireNonNull(responseBody);
            responseEffects = Set.copyOf(responseEffects);
        }
    }
}
