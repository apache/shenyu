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

package org.apache.shenyu.plugin.agent.gateway.resource;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolInvocation;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentResourceRegistryTest {

    @Test
    void shouldListDirectAndTemplateMetadataSeparatelyWithDefensiveCopies() {
        AgentResourceRegistry registry = new AgentResourceRegistry(List.of(provider("text", "test://text", false, new AtomicInteger()),
                provider("template", "test://template/{id}/data", true, new AtomicInteger())));
        JsonArray direct = registry.list(Set.of("text", "template"), false);
        assertEquals(1, direct.size());
        assertEquals("test://text", direct.get(0).getAsJsonObject().get("uri").getAsString());
        direct.get(0).getAsJsonObject().addProperty("uri", "changed");
        assertEquals("test://text", registry.list(Set.of("text"), false).get(0).getAsJsonObject().get("uri").getAsString());
        assertEquals("test://template/{id}/data", registry.list(Set.of("template"), true).get(0).getAsJsonObject().get("uriTemplate").getAsString());
        assertEquals(0, registry.list(Set.of(), false).size());
        assertFalse(registry.isEmpty());
    }

    @Test
    void shouldReadTemplateLazilyAndCaptureTrustedIdentity() {
        AtomicInteger reads = new AtomicInteger();
        AgentResourceRegistry registry = new AgentResourceRegistry(List.of(provider("template", "test://template/{id}/data", true, reads)));
        Mono<JsonArray> pending = registry.read("test://template/123/data", Set.of("template"), () -> context("owner"));
        assertEquals(0, reads.get());
        StepVerifier.create(pending).assertNext(result -> {
            assertEquals("owner:123", result.get(0).getAsJsonObject().get("text").getAsString());
            assertEquals("test://template/123/data", result.get(0).getAsJsonObject().get("uri").getAsString());
            result.get(0).getAsJsonObject().addProperty("text", "changed");
        }).verifyComplete();
        StepVerifier.create(pending).assertNext(result -> assertEquals("owner:123", result.get(0).getAsJsonObject().get("text").getAsString())).verifyComplete();
        assertEquals(2, reads.get());
    }

    @Test
    void shouldDenyBeforeProviderOrContextFactoryForUnregisteredAndUnauthorizedUris() {
        AtomicInteger reads = new AtomicInteger();
        AtomicInteger contexts = new AtomicInteger();
        AgentResourceRegistry registry = new AgentResourceRegistry(List.of(provider("text", "test://text", false, reads)));
        for (String uri : List.of("test://text", "test://unknown", "https://127.0.0.1:1/never-fetch", "file:///private/key")) {
            StepVerifier.create(registry.read(uri, Set.of(), () -> {
                contexts.incrementAndGet();
                return context("owner");
            })).expectError(SecurityException.class).verify();
        }
        assertEquals(0, reads.get());
        assertEquals(0, contexts.get());
    }

    @Test
    void shouldCancelReadAndNotDetachSubscription() {
        AtomicBoolean started = new AtomicBoolean();
        AtomicBoolean cancelled = new AtomicBoolean();
        AgentResourceProvider provider = new AgentResourceProvider() {
            @Override
            public AgentResourceDefinition getDefinition() {
                return new AgentResourceDefinition("text", "test://text", "Local", null, false);
            }

            @Override
            public Mono<List<AgentResourceContent>> read(final AgentResourceInvocation invocation) {
                return Mono.<List<AgentResourceContent>>never().doOnSubscribe(subscription -> started.set(true)).doOnCancel(() -> cancelled.set(true));
            }
        };
        StepVerifier.create(new AgentResourceRegistry(List.of(provider)).read("test://text", Set.of("text"), () -> context("owner")))
                .then(() -> assertTrue(started.get())).thenCancel().verify();
        assertTrue(cancelled.get());
    }

    @Test
    void shouldRejectEmptyContentAndEmptyMono() {
        for (List<AgentResourceContent> content : List.of(List.<AgentResourceContent>of())) {
            AgentResourceProvider provider = resultProvider(Mono.just(content));
            StepVerifier.create(new AgentResourceRegistry(List.of(provider)).read("test://text", Set.of("text"), () -> context("owner")))
                    .expectError(IllegalStateException.class).verify();
        }
        StepVerifier.create(new AgentResourceRegistry(List.of(resultProvider(Mono.empty()))).read("test://text", Set.of("text"), () -> context("owner")))
                .expectError(IllegalStateException.class).verify();
    }

    @Test
    void shouldRejectDuplicateNamesUnsafeUrisAndUnsupportedTemplateOperators() {
        AgentResourceProvider provider = provider("text", "test://text", false, new AtomicInteger());
        assertThrows(IllegalArgumentException.class, () -> new AgentResourceRegistry(List.of(provider, provider)));
        assertThrows(IllegalArgumentException.class, () -> new AgentResourceDefinition("text", "relative/path", "", null, false));
        assertThrows(IllegalArgumentException.class, () -> new AgentResourceDefinition("text", "https://user:secret@host", "", null, false));
        assertThrows(IllegalArgumentException.class, () -> new AgentResourceDefinition("text", "test://template/{+path}", "", null, true));
        assertThrows(IllegalArgumentException.class, () -> new AgentResourceDefinition("text", "test://text", "", null, true));
    }

    @Test
    void shouldNotExposeMutableTemplateVariables() {
        AgentResourceDefinition definition = new AgentResourceDefinition("template", "test://template/{id}/data", "Local", null, true);
        assertThrows(UnsupportedOperationException.class, () -> definition.variables("test://template/123/data").put("id", "other"));
        assertEquals(Map.of("id", "123"), definition.variables("test://template/123/data"));
        assertFalse(definition.matches("test://template/123/other"));
    }

    @Test
    void shouldRejectAmbiguousAuthorizedUrisBeforeExecutingProviders() {
        AtomicInteger reads = new AtomicInteger();
        AgentResourceRegistry registry = new AgentResourceRegistry(List.of(provider("direct", "test://template/123/data", false, reads),
                provider("template", "test://template/{id}/data", true, reads)));
        StepVerifier.create(registry.read("test://template/123/data", Set.of("direct", "template"), () -> context("owner")))
                .expectError(IllegalStateException.class).verify();
        assertEquals(0, reads.get());
        StepVerifier.create(registry.read("test://template/123/data", Set.of("direct"), () -> context("owner")))
                .assertNext(result -> assertEquals("owner:direct", result.get(0).getAsJsonObject().get("text").getAsString())).verifyComplete();
        assertEquals(1, reads.get());
    }

    private AgentToolInvocation context(final String subject) {
        return new AgentToolInvocation("internal", subject, new JsonObject());
    }

    private AgentResourceProvider resultProvider(final Mono<List<AgentResourceContent>> result) {
        return new AgentResourceProvider() {
            @Override
            public AgentResourceDefinition getDefinition() {
                return new AgentResourceDefinition("text", "test://text", "Local", null, false);
            }

            @Override
            public Mono<List<AgentResourceContent>> read(final AgentResourceInvocation invocation) {
                return result;
            }
        };
    }

    private AgentResourceProvider provider(final String name, final String uri, final boolean template, final AtomicInteger reads) {
        return new AgentResourceProvider() {
            @Override
            public AgentResourceDefinition getDefinition() {
                return new AgentResourceDefinition(name, uri, "Local", "text/plain", template);
            }

            @Override
            public Mono<List<AgentResourceContent>> read(final AgentResourceInvocation invocation) {
                reads.incrementAndGet();
                String text = invocation.getContext().getSubject() + ":" + invocation.getVariables().getOrDefault("id", "direct");
                return Mono.just(List.of(AgentResourceContent.text(invocation.getUri(), "text/plain", text)));
            }
        };
    }
}
