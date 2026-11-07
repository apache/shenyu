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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/** A small optional companion catalog; it never loads or executes plugin implementations. */
public final class PluginCapabilityRegistry {

    private final Map<Implementation, PluginCapabilityDescriptor> declarations = new ConcurrentHashMap<>();

    /**
     * Built-in HTTP pilot declarations and explicitly registered extension declarations.
     * @return shared catalog
     */
    public static PluginCapabilityRegistry shared() {
        return Shared.INSTANCE;
    }

    /**
     * Register static metadata, independently of any plugin Enum or configuration schema.
     * Ambiguous versions require rebuilding the catalog rather than silently replacing facts.
     * @param descriptor trusted immutable declaration
     */
    public synchronized void register(final PluginCapabilityDescriptor descriptor) {
        Implementation key = new Implementation(descriptor.plugin(), descriptor.implementation());
        if (declarations.size() >= 256) {
            throw new IllegalArgumentException("Plugin capability catalog limit exceeded");
        }
        if (Objects.nonNull(declarations.putIfAbsent(key, descriptor))) {
            throw new IllegalArgumentException("Duplicate plugin capability declaration: " + descriptor.plugin());
        }
    }

    /**
     * Find an exact implementation declaration; subclasses do not inherit unsupported promises.
     * @param plugin plugin identity
     * @param implementation implementation class name
     * @return declaration, or explicit absence
     */
    public Optional<PluginCapabilityDescriptor> find(final String plugin, final String implementation) {
        return Optional.ofNullable(declarations.get(new Implementation(plugin, implementation)));
    }

    /**
     * Read registered declarations. Presence here alone does not prove installation.
     * @return immutable deterministic catalog
     */
    public List<PluginCapabilityDescriptor> catalog() {
        return declarations.values().stream().sorted(Comparator.comparing(PluginCapabilityDescriptor::plugin)
                .thenComparing(PluginCapabilityDescriptor::implementation)).toList();
    }

    /**
     * Compare explicit observed implementations and supporting capabilities without execution.
     * This is an installation-level preflight, not pipeline ordering, matching, readiness or auth validation.
     * @param observed bounded observations supplied by application wiring
     * @param supporting separately observed provider capabilities, never guessed from request configuration
     * @param context protocol/context to inspect
     * @return bounded conservative findings
     */
    public Preflight preflight(final List<Implementation> observed, final Set<String> supporting, final String context) {
        if (observed.size() > 256 || supporting.size() > 256) {
            throw new IllegalArgumentException("Plugin capability preflight input limit exceeded");
        }
        Set<String> available = new HashSet<>();
        for (Implementation implementation : observed) {
            Optional.ofNullable(declarations.get(implementation)).ifPresent(value -> available.addAll(value.provides()));
        }
        List<Finding> findings = new ArrayList<>();
        for (Implementation implementation : observed) {
            PluginCapabilityDescriptor descriptor = declarations.get(implementation);
            if (Objects.isNull(descriptor)) {
                findings.add(new Finding(implementation.plugin(), "UNKNOWN_DESCRIPTOR", ""));
            } else {
                PluginCapabilityDescriptor.Support support = descriptor.contexts().get(context);
                if (Objects.isNull(support) || support == PluginCapabilityDescriptor.Support.UNKNOWN) {
                    findings.add(new Finding(implementation.plugin(), "UNKNOWN_CONTEXT", ""));
                } else if (support == PluginCapabilityDescriptor.Support.UNSUPPORTED) {
                    findings.add(new Finding(implementation.plugin(), "UNSUPPORTED_CONTEXT", ""));
                }
                descriptor.requires().stream().filter(required -> !available.contains(required))
                        .forEach(required -> findings.add(new Finding(implementation.plugin(), "MISSING_EXECUTION_CAPABILITY", required)));
                descriptor.supportingRequirements().stream().filter(required -> !supporting.contains(required))
                        .forEach(required -> findings.add(new Finding(implementation.plugin(), "UNRESOLVED_SUPPORTING_CAPABILITY", required)));
            }
            if (findings.size() >= 128) {
                return new Preflight(List.copyOf(findings.subList(0, 128)), true, "installation observations only; request selection unknown");
            }
        }
        return new Preflight(findings, false, "installation observations only; request selection unknown");
    }

    /** Exact identity, allowing external plugin names without modifying PluginEnum. */
    public record Implementation(String plugin, String implementation) {
        public Implementation {
            if (Objects.isNull(plugin) || plugin.length() > 64 || Objects.isNull(implementation) || implementation.length() > 256) {
                throw new IllegalArgumentException("Plugin capability observation identity limit exceeded");
            }
        }
    }

    /**
     * Bounded diagnostic fact, containing no runtime handles or request data.
     * @param plugin observed implementation identity
     * @param code stable availability classification
     * @param requirement unresolved capability, or empty
     */
    public record Finding(String plugin, String code, String requirement) {
    }

    /**
     * Conservative read-only comparison.
     * @param findings bounded unknown/unsupported/missing requirements
     * @param truncated whether findings were capped
     * @param scope observation scope, never a readiness claim
     */
    public record Preflight(List<Finding> findings, boolean truncated, String scope) {
        public Preflight {
            findings = List.copyOf(findings);
        }
    }

    private static final class Shared {

        private static final PluginCapabilityRegistry INSTANCE = create();

        private static PluginCapabilityRegistry create() {
            PluginCapabilityRegistry registry = new PluginCapabilityRegistry();
            HttpPilotCapabilities.declarations().forEach(registry::register);
            return registry;
        }
    }
}
