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

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PluginCapabilityRegistryTest {

    @Test
    void optionalDeclarationsDoNotLoadImplementationClassesOrRequireEnumChanges() {
        PluginCapabilityRegistry registry = new PluginCapabilityRegistry();
        PluginCapabilityDescriptor descriptor = external("1");
        registry.register(descriptor);
        assertEquals(descriptor, registry.find("external-plugin", "not.on.classpath.Plugin").orElseThrow());
        assertTrue(registry.find("legacy-plugin", "not.on.classpath.Legacy").isEmpty());
        assertThrows(UnsupportedOperationException.class, () -> registry.catalog().clear());
        assertThrows(UnsupportedOperationException.class, () -> descriptor.contexts().clear());
        assertThrows(UnsupportedOperationException.class, () -> descriptor.modes().clear());
    }

    @Test
    void identityAndVersionCollisionsFailWithoutOverwritingDeclarations() {
        PluginCapabilityRegistry registry = new PluginCapabilityRegistry();
        registry.register(external("1"));
        assertThrows(IllegalArgumentException.class, () -> registry.register(external("1")));
        assertThrows(IllegalArgumentException.class, () -> registry.register(external("2")));
        assertEquals("1", registry.catalog().get(0).version());
        PluginCapabilityDescriptor original = external("1");
        assertThrows(IllegalArgumentException.class, () -> new PluginCapabilityDescriptor(2, original.version(), original.plugin(),
                original.implementation(), original.contexts(), original.modes(), original.provides(), original.requires(),
                original.supportingRequirements(), null, original.provenance()));
    }

    @Test
    void fullCooperatingHttpPilotHasConservativePreflightWithoutRequestExecution() {
        PluginCapabilityRegistry registry = PluginCapabilityRegistry.shared();
        List<PluginCapabilityRegistry.Implementation> pipeline = List.of(identity("cache", "cache.CachePlugin"),
                identity("divide", "divide.DividePlugin"), identity("uri", "uri.URIPlugin"),
                identity("nettyHttpClient", "httpclient.NettyHttpClientPlugin"), identity("response", "response.ResponsePlugin"));
        var report = registry.preflight(pipeline, Set.of("load-balancer", "http-writer-binding", "response-cache-provider"), "http");
        assertTrue(report.findings().isEmpty());
        assertFalse(report.truncated());
        assertTrue(report.scope().contains("request selection unknown"));
        var missingWriter = registry.preflight(pipeline.subList(0, pipeline.size() - 1), Set.of(), "http");
        assertTrue(missingWriter.findings().stream().anyMatch(finding -> "http.writer".equals(finding.requirement())));
        assertTrue(missingWriter.findings().stream().anyMatch(finding -> "UNRESOLVED_SUPPORTING_CAPABILITY".equals(finding.code())));
        var unknown = registry.preflight(List.of(new PluginCapabilityRegistry.Implementation("legacy", "external.Legacy")), Set.of(), "http");
        assertEquals("UNKNOWN_DESCRIPTOR", unknown.findings().get(0).code());
        assertFalse(registry.preflight(pipeline, Set.of(), "not-audited").findings().isEmpty());
    }

    @Test
    void pilotKeepsConditionalCacheAndClientBodyRolesSeparate() {
        PluginCapabilityRegistry registry = PluginCapabilityRegistry.shared();
        PluginCapabilityDescriptor divide = registry.find("divide", "org.apache.shenyu.plugin.divide.DividePlugin").orElseThrow();
        assertEquals(Set.of("http.domain"), divide.provides());
        assertFalse(divide.provides().contains("http.result"));
        PluginCapabilityDescriptor cache = registry.find("cache", "org.apache.shenyu.plugin.cache.CachePlugin").orElseThrow();
        assertTrue(cache.modes().stream().anyMatch(mode -> mode.responseEffects().contains(PluginCapabilityDescriptor.ResponseEffect.SHORT_CIRCUIT)));
        assertTrue(cache.modes().stream().anyMatch(mode -> mode.responseEffects().contains(PluginCapabilityDescriptor.ResponseEffect.DECORATE)));
        PluginCapabilityDescriptor client = registry.find("nettyHttpClient", "org.apache.shenyu.plugin.httpclient.NettyHttpClientPlugin").orElseThrow();
        assertTrue(client.modes().stream().anyMatch(mode -> mode.requestBody() == PluginCapabilityDescriptor.Body.BOUNDED_BUFFER));
        assertEquals(PluginCapabilityDescriptor.Support.UNKNOWN, client.contexts().get("other"));
    }

    private PluginCapabilityRegistry.Implementation identity(final String name, final String implementation) {
        return new PluginCapabilityRegistry.Implementation(name, "org.apache.shenyu.plugin." + implementation);
    }

    private PluginCapabilityDescriptor external(final String version) {
        return new PluginCapabilityDescriptor(1, version, "external-plugin", "not.on.classpath.Plugin",
                Map.of("http", PluginCapabilityDescriptor.Support.UNKNOWN),
                List.of(new PluginCapabilityDescriptor.Mode("not audited", PluginCapabilityDescriptor.Body.UNKNOWN,
                        PluginCapabilityDescriptor.Body.UNKNOWN, Set.of())),
                Set.of(), Set.of(), Set.of(), null, "external static companion; no runtime configuration");
    }
}
