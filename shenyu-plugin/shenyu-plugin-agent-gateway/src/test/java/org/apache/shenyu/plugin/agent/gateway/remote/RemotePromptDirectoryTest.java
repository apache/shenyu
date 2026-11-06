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
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RemotePromptDirectoryTest {

    private final ObjectMapper json = new ObjectMapper();

    @Test
    void fullLengthUpstreamNamesRemainGrantableAfterPrefixing() {
        String server = "s".repeat(48);
        String original = "p".repeat(128);
        String exposed = server + "." + original;
        RemotePromptDirectory directory = RemotePromptDirectory.discover(List.of(target(server, page(original, null)))).block();
        assertEquals(Set.of(exposed), directory.definitions().keySet());
        directory.get(exposed, Set.of(exposed), Map.of("topic", "gateway")).block();
        ObjectNode invalid = page("x".repeat(129), null);
        invalid.withArray("prompts").add(page("valid", null).path("prompts").get(0));
        assertEquals(Set.of("server.valid"), RemotePromptDirectory.discover(List.of(target("server", invalid))).block().definitions().keySet());
    }

    @Test
    void discoveryCapturesTargetsBeforeSubscription() {
        List<RemotePromptDirectory.Target> targets = new ArrayList<>(List.of(target("first", page("summary", null))));
        Mono<RemotePromptDirectory> discovery = RemotePromptDirectory.discover(targets);
        targets.clear();
        assertEquals(Set.of("first.summary"), discovery.block().definitions().keySet());
        assertEquals(Set.of("first.summary"), discovery.block().definitions().keySet());
    }

    @Test
    void namespacesAllServersPagesAndRoutesToOriginalNames() {
        AtomicReference<String> called = new AtomicReference<>();
        RemotePromptDirectory.Target first = new RemotePromptDirectory.Target("first", cursor ->
                Mono.just(page(Objects.isNull(cursor) ? "summary" : "next", Objects.isNull(cursor) ? "second" : null)),
                (name, arguments) -> {
                    called.set(name + ":" + arguments.get("topic"));
                    return Mono.just(result("first"));
                });
        RemotePromptDirectory.Target second = target("second", page("summary", null));
        RemotePromptDirectory directory = RemotePromptDirectory.discover(List.of(first, second)).block();
        assertEquals(Set.of("first.summary", "first.next", "second.summary"), directory.definitions().keySet());
        directory.definitions().get("first.summary").put("name", "forged");
        assertEquals("first.summary", directory.definitions().get("first.summary").path("name").asText());
        assertEquals("first", directory.get("first.summary", Set.of("first.summary"), Map.of("topic", "gateway"))
                .block().path("messages").get(0).path("content").path("text").asText());
        assertEquals("summary:gateway", called.get());
        StepVerifier.create(directory.get("second.summary", Set.of("first.summary"), Map.of("topic", "gateway")))
                .expectError(SecurityException.class).verify();
        assertEquals("summary:gateway", called.get());
    }

    @Test
    void repeatedCursorsDuplicatesAndExcessTargetsFailClosed() {
        StepVerifier.create(RemotePromptDirectory.discover(List.of(target("first", page("summary", "again")))))
                .expectError(IllegalArgumentException.class).verify();
        ObjectNode duplicate = page("summary", null);
        duplicate.withArray("prompts").add(duplicate.path("prompts").get(0).deepCopy());
        StepVerifier.create(RemotePromptDirectory.discover(List.of(target("first", duplicate))))
                .expectError(IllegalArgumentException.class).verify();
        StepVerifier.create(RemotePromptDirectory.discover(java.util.Collections.nCopies(9, target("first", page("summary", null)))))
                .expectError(IllegalArgumentException.class).verify();
        assertThrows(IllegalArgumentException.class, () -> target("unsafe.server", page("summary", null)));
    }

    @Test
    void pageBoundsEmptyMalformedAndOversizedMetadataCannotPublish() {
        AtomicInteger page = new AtomicInteger();
        RemotePromptDirectory.Target endless = new RemotePromptDirectory.Target("first",
                cursor -> Mono.just(page("p" + page.incrementAndGet(), "next-" + page.get())), (name, arguments) -> Mono.just(result(name)));
        StepVerifier.create(RemotePromptDirectory.discover(List.of(endless))).expectError(IllegalArgumentException.class).verify();
        assertEquals(4, page.get());
        StepVerifier.create(RemotePromptDirectory.discover(List.of(new RemotePromptDirectory.Target("first", cursor -> Mono.empty(),
                (name, arguments) -> Mono.empty())))).expectError(IllegalStateException.class).verify();
        ObjectNode malformed = json.createObjectNode();
        malformed.putArray("prompts").add(1);
        StepVerifier.create(RemotePromptDirectory.discover(List.of(target("first", malformed)))).expectError(IllegalArgumentException.class).verify();
        ObjectNode large = page("summary", null);
        ((ObjectNode) large.path("prompts").get(0)).put("description", "x".repeat(4097));
        StepVerifier.create(RemotePromptDirectory.discover(List.of(target("first", large)))).expectError(IllegalArgumentException.class).verify();
    }

    @Test
    void snapshotReplacementDoesNotMutatePriorDefinitionsAndCancellationReachesCallback() {
        AtomicInteger cancelled = new AtomicInteger();
        RemotePromptDirectory first = RemotePromptDirectory.discover(List.of(new RemotePromptDirectory.Target("first",
                cursor -> Mono.just(page("old", null)), (name, arguments) -> Mono.<ObjectNode>never().doOnCancel(cancelled::incrementAndGet)))).block();
        RemotePromptDirectory next = RemotePromptDirectory.discover(List.of(target("first", page("new", null)))).block();
        assertEquals(Set.of("first.old"), first.definitions().keySet());
        assertEquals(Set.of("first.new"), next.definitions().keySet());
        StepVerifier.create(first.get("first.old", Set.of("first.old"), Map.of("topic", "a"))).thenCancel().verify();
        assertEquals(1, cancelled.get());
        StepVerifier.create(next.get("first.old", Set.of("first.old"), Map.of("topic", "a"))).expectError(SecurityException.class).verify();
        assertTrue(RemotePromptDirectory.empty().definitions().isEmpty());
    }

    private RemotePromptDirectory.Target target(final String name, final ObjectNode page) {
        return new RemotePromptDirectory.Target(name, cursor -> Mono.just(page.deepCopy()), (original, arguments) -> Mono.just(result(name)));
    }

    private ObjectNode page(final String name, final String next) {
        ObjectNode page = json.createObjectNode();
        ObjectNode prompt = page.putArray("prompts").addObject().put("name", name);
        prompt.putArray("arguments").addObject().put("name", "topic").put("required", true);
        if (Objects.nonNull(next)) {
            page.put("nextCursor", next);
        }
        return page;
    }

    private ObjectNode result(final String text) {
        ObjectNode result = json.createObjectNode();
        result.putArray("messages").addObject().put("role", "assistant").putObject("content").put("type", "text").put("text", text);
        return result;
    }
}
