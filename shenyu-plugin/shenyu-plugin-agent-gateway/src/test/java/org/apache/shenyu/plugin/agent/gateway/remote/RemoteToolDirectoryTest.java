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
import io.modelcontextprotocol.spec.McpSchema;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RemoteToolDirectoryTest {

    private static final Duration WAIT = Duration.ofSeconds(5);

    private static final ObjectMapper JSON = new ObjectMapper();

    @Test
    void serialAndBoundedDiscoveryProduceIdenticalDirectories() {
        List<RemoteToolDirectory.Target> source = List.of(target("alpha"), target("beta"), target("gamma"));
        RemoteToolDirectory serial = RemoteToolDirectory.discover(source, 4, 16, 1).block(WAIT);
        RemoteToolDirectory bounded = RemoteToolDirectory.discover(source, 4, 16, 3).block(WAIT);
        assertNotNull(serial);
        assertNotNull(bounded);
        assertEquals(serial.names(), bounded.names());
        assertEquals(Set.of("alpha.lookup", "beta.lookup", "gamma.lookup"), bounded.names());
    }

    @Test
    void boundedConcurrencyOverlapsIndependentTargetDiscovery() {
        int targets = 4;
        // Every target blocks until all four have been subscribed; only genuine overlap can release the barrier.
        CountDownLatch barrier = new CountDownLatch(targets);
        List<RemoteToolDirectory.Target> source = new ArrayList<>();
        for (int index = 0; index < targets; index++) {
            String name = "srv" + index;
            source.add(new RemoteToolDirectory.Target(
                name,
                cursor -> Mono.fromCallable(() -> {
                    barrier.countDown();
                    assertTrue(barrier.await(3, TimeUnit.SECONDS), "targets were not discovered concurrently");
                    return page("lookup");
                }).subscribeOn(Schedulers.boundedElastic()),
                request -> Mono.error(new UnsupportedOperationException()),
                request -> Mono.error(new UnsupportedOperationException())
            ));
        }
        RemoteToolDirectory directory = RemoteToolDirectory.discover(source, 4, 16, targets).block(WAIT);
        assertNotNull(directory);
        assertEquals(targets, directory.names().size());
        assertEquals(0, barrier.getCount());
    }

    @Test
    void defaultDiscoverOverloadStaysSerialized() {
        var first = reactor.core.publisher.Sinks.<McpSchema.ListToolsResult>one();
        var second = reactor.core.publisher.Sinks.<McpSchema.ListToolsResult>one();
        List<RemoteToolDirectory.Target> source = List.of(
            new RemoteToolDirectory.Target(
                "alpha", cursor -> first.asMono(),
                request -> Mono.error(new UnsupportedOperationException()),
                request -> Mono.error(new UnsupportedOperationException())
            ),
            new RemoteToolDirectory.Target(
                "beta", cursor -> second.asMono(),
                request -> Mono.error(new UnsupportedOperationException()),
                request -> Mono.error(new UnsupportedOperationException())
            )
        );
        var pending = RemoteToolDirectory.discover(source, 4, 16).toFuture();
        try {
            assertEquals(1, first.currentSubscriberCount());
            assertEquals(0, second.currentSubscriberCount());
            first.tryEmitValue(page("lookup"));
            assertEquals(1, second.currentSubscriberCount());
            second.tryEmitValue(page("lookup"));
            RemoteToolDirectory directory = Mono.fromFuture(pending).block(WAIT);
            assertNotNull(directory);
            assertEquals(Set.of("alpha.lookup", "beta.lookup"), directory.names());
        } finally {
            pending.cancel(true);
        }
    }

    @Test
    void duplicateServerNameIsRejectedRegardlessOfConcurrency() {
        List<RemoteToolDirectory.Target> source = List.of(target("dup"), target("dup"));
        assertThrows(IllegalArgumentException.class, () -> RemoteToolDirectory.discover(source, 4, 16, 2).block(WAIT));
    }

    @Test
    void totalToolLimitIsEnforcedUnderBoundedConcurrency() {
        List<RemoteToolDirectory.Target> source = List.of(target("a"), target("b"), target("c"));
        assertThrows(IllegalArgumentException.class, () -> RemoteToolDirectory.discover(source, 4, 2, 3).block(WAIT));
    }

    @Test
    void concurrencyOutsideBoundedRangeIsRejected() {
        List<RemoteToolDirectory.Target> source = List.of(target("alpha"));
        assertThrows(IllegalArgumentException.class, () -> RemoteToolDirectory.discover(source, 4, 16, 0).block(WAIT));
        assertThrows(IllegalArgumentException.class, () -> RemoteToolDirectory.discover(source, 4, 16, 9).block(WAIT));
    }

    private static RemoteToolDirectory.Target target(final String name) {
        return new RemoteToolDirectory.Target(
            name,
            cursor -> Mono.just(page("lookup")),
            request -> Mono.error(new UnsupportedOperationException()),
            request -> Mono.error(new UnsupportedOperationException())
        );
    }

    private static McpSchema.ListToolsResult page(final String toolName) {
        Map<String, Object> tool = Map.of("name", toolName, "description", "unit tool",
            "inputSchema", Map.of("type", "object", "properties", Map.of("token", Map.of("type", "string"))));
        ObjectNode node = JSON.createObjectNode();
        node.set("tools", JSON.valueToTree(List.of(tool)));
        return JSON.convertValue(node, McpSchema.ListToolsResult.class);
    }
}
