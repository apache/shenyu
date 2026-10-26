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

package org.apache.shenyu.plugin.agent.gateway.remote;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceDefinition;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceNamespace;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RemoteResourceDirectoryTest {

    private static final Duration WAIT = Duration.ofSeconds(3);

    private final ObjectMapper mapper = new ObjectMapper();

    @Test
    void sameNameAndOriginalUriAreSeparatedByServerAndReadsUseOriginalUri() {
        AtomicInteger reads = new AtomicInteger();
        var directory = RemoteResourceDirectory.discover(List.of(target("orders", false, reads), target("inventory", false, reads))).block(WAIT);
        assertEquals(Set.of("orders.status", "inventory.status"), directory.definitions().keySet());
        for (String server : List.of("orders", "inventory")) {
            String uri = directory.definitions().get(server + ".status").path("uri").asText();
            ObjectNode result = directory.read(uri, Set.of(server + ".status")).block(WAIT);
            assertEquals(uri, result.path("contents").get(0).path("uri").asText());
            assertEquals(server + ":test://status", result.path("contents").get(0).path("text").asText());
        }
        assertEquals(2, reads.get());
    }

    @Test
    void permissionAndExactRegistrationAreRequiredBeforeRead() {
        AtomicInteger reads = new AtomicInteger();
        var directory = RemoteResourceDirectory.discover(List.of(target("orders", false, reads))).block(WAIT);
        String uri = directory.definitions().get("orders.status").path("uri").asText();
        for (String candidate : List.of(uri, "file:///secret", "https://127.0.0.1:1/private", AgentResourceNamespace.expose("orders", "test://unknown", false))) {
            StepVerifier.create(directory.read(candidate, Set.of())).expectError(SecurityException.class).verify(WAIT);
        }
        StepVerifier.create(directory.read(uri, Set.of("inventory.status"))).expectError(SecurityException.class).verify(WAIT);
        assertEquals(0, reads.get());
    }

    @Test
    void metadataAndResultsCannotMutateTheFrozenDirectory() {
        var directory = RemoteResourceDirectory.discover(List.of(target("orders", false, new AtomicInteger()))).block(WAIT);
        String uri = directory.definitions().get("orders.status").path("uri").asText();
        directory.definitions().get("orders.status").put("uri", "changed");
        assertEquals(uri, directory.definitions().get("orders.status").path("uri").asText());
        directory.read(uri, Set.of("orders.status")).block(WAIT).withArray("contents").removeAll();
        assertEquals(1, directory.read(uri, Set.of("orders.status")).block(WAIT).path("contents").size());
    }

    @Test
    void simpleTemplatesRoundTripEncodedIdentifiers() {
        var directory = RemoteResourceDirectory.discover(List.of(target("orders", true, new AtomicInteger()))).block(WAIT);
        String template = directory.definitions().get("orders.status").path("uriTemplate").asText();
        String uri = AgentResourceNamespace.expose("orders", "test://status/123", false);
        assertTrue(new AgentResourceDefinition("status", template, "", null, true).matches(uri));
        assertEquals("test://status/123", AgentResourceNamespace.original("orders", uri));
        assertEquals("orders:test://status/123", directory.read(uri, Set.of("orders.status")).block(WAIT).path("contents").get(0).path("text").asText());
        String encoded = AgentResourceNamespace.expose("orders", "test://status/%2F", false);
        assertEquals("test://status/%2F", AgentResourceNamespace.original("orders", encoded));
        assertThrows(IllegalArgumentException.class, () -> AgentResourceNamespace.original("inventory", encoded));
    }

    @Test
    void repeatedAndUnboundedCursorsFailWithoutPublishingPartialMetadata() {
        for (boolean repeated : List.of(true, false)) {
            AtomicInteger pages = new AtomicInteger();
            var target = new RemoteResourceDirectory.Target("orders", cursor -> {
                ObjectNode result = page("resources");
                result.put("nextCursor", repeated ? "same" : "page-" + pages.incrementAndGet());
                return Mono.just(result);
            }, cursor -> Mono.just(page("resourceTemplates")), uri -> Mono.error(new AssertionError("No reads")));
            StepVerifier.create(RemoteResourceDirectory.discover(List.of(target))).expectError(IllegalArgumentException.class).verify(WAIT);
            assertTrue(pages.get() <= 4);
        }
    }

    @Test
    void duplicateNamesOrUrisInOneServerFailClosed() {
        var duplicate = new RemoteResourceDirectory.Target("orders", cursor -> {
            ObjectNode page = resourcePage(false);
            page.withArray("resources").add(page.withArray("resources").get(0).deepCopy());
            return Mono.just(page);
        }, cursor -> Mono.just(page("resourceTemplates")), uri -> Mono.empty());
        StepVerifier.create(RemoteResourceDirectory.discover(List.of(duplicate))).expectError(IllegalArgumentException.class).verify(WAIT);
    }

    @Test
    void rejectsAnotherResourceInReadResultAndMalformedBinary() {
        for (String variant : List.of("uri", "blob", "empty")) {
            var target = new RemoteResourceDirectory.Target("orders", cursor -> Mono.just(resourcePage(false)),
                    cursor -> Mono.just(page("resourceTemplates")), uri -> {
                        ObjectNode result = contents(uri, "body");
                        ObjectNode item = (ObjectNode) result.path("contents").get(0);
                        if ("uri".equals(variant)) {
                            item.put("uri", "file:///another");
                        } else if ("blob".equals(variant)) {
                            item.remove("text");
                            item.put("blob", "%%%");
                        } else {
                            result.withArray("contents").removeAll();
                        }
                        return Mono.just(result);
                    });
            var directory = RemoteResourceDirectory.discover(List.of(target)).block(WAIT);
            StepVerifier.create(directory.read(AgentResourceNamespace.expose("orders", "test://status", false), Set.of("orders.status")))
                    .expectError(IllegalArgumentException.class).verify(WAIT);
        }
    }

    @Test
    void cancellationReachesOnlyTheCurrentRead() {
        AtomicInteger active = new AtomicInteger();
        AtomicBoolean cancelled = new AtomicBoolean();
        Function<String, Mono<ObjectNode>> read = uri -> Mono.<ObjectNode>never().doOnSubscribe(ignored -> active.incrementAndGet())
                .doOnCancel(() -> cancelled.set(true)).doFinally(ignored -> active.decrementAndGet());
        var target = new RemoteResourceDirectory.Target("orders", cursor -> Mono.just(resourcePage(false)),
                cursor -> Mono.just(page("resourceTemplates")), read);
        var directory = RemoteResourceDirectory.discover(List.of(target)).block(WAIT);
        StepVerifier.create(directory.read(AgentResourceNamespace.expose("orders", "test://status", false), Set.of("orders.status")))
                .then(() -> assertEquals(1, active.get())).thenCancel().verify(WAIT);
        assertTrue(cancelled.get());
        assertEquals(0, active.get());
    }

    private RemoteResourceDirectory.Target target(final String server, final boolean template, final AtomicInteger reads) {
        return new RemoteResourceDirectory.Target(server, cursor -> Mono.just(template ? page("resources") : resourcePage(false)),
                cursor -> Mono.just(template ? resourcePage(true) : page("resourceTemplates")), uri -> {
                    reads.incrementAndGet();
                    return Mono.just(contents(uri, server + ":" + uri));
                });
    }

    private ObjectNode resourcePage(final boolean template) {
        String field = template ? "resourceTemplates" : "resources";
        ObjectNode result = page(field);
        result.withArray(field).addObject().put("name", "status").put(template ? "uriTemplate" : "uri", template ? "test://status/{id}" : "test://status");
        return result;
    }

    private ObjectNode page(final String field) {
        ObjectNode result = mapper.createObjectNode();
        result.putArray(field);
        return result;
    }

    private ObjectNode contents(final String uri, final String text) {
        ObjectNode result = mapper.createObjectNode();
        result.putArray("contents").addObject().put("uri", uri).put("text", text);
        return result;
    }
}
