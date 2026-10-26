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

package org.apache.shenyu.protocol.mqtt.repositories;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Tracks the inbound packet identifiers currently being acknowledged, so that
 * a retransmitted (DUP) publish can be answered without re-delivering it (#7919).
 * Entries are released by PUBREL, and a new session (clean-session connect or no
 * stored session) discards them, since it resets the packet id space.
 *
 * <p>The state is keyed by the client id, not by the channel: session state
 * belongs to the client id per spec, so during a duplicate-clientId takeover the
 * replacement channel shares — and must keep honoring — the same in-flight ids.</p>
 *
 * <p>{@link #register} and {@link #release} each run inside a single per-key
 * {@code compute}, so a packet id added concurrently with an empty-entry cleanup
 * can never be evicted with it, and an id is never added to a set that the map
 * has already dropped (#8021).</p>
 */
public class InflightRepository implements BaseRepository<String, Set<Integer>> {

    private final Map<String, Set<Integer>> inboundInflight = new ConcurrentHashMap<>();

    @Override
    public void add(final String clientId, final Set<Integer> packetIds) {
        inboundInflight.put(clientId, packetIds);
    }

    @Override
    public void remove(final String clientId) {
        inboundInflight.remove(clientId);
    }

    @Override
    public Set<Integer> get(final String clientId) {
        return inboundInflight.get(clientId);
    }

    /**
     * Registers a packet identifier as being acknowledged.
     *
     * @param clientId the client that published the packet
     * @param packetId the packet identifier of the inbound publish
     * @return true when the packet was seen for the first time, false on a retransmission
     */
    public boolean register(final String clientId, final int packetId) {
        final AtomicBoolean firstSeen = new AtomicBoolean();
        inboundInflight.compute(clientId, (key, packetIds) -> {
            final Set<Integer> ids = Objects.nonNull(packetIds) ? packetIds : ConcurrentHashMap.newKeySet();
            firstSeen.set(ids.add(packetId));
            return ids;
        });
        return firstSeen.get();
    }

    /**
     * Releases the packet identifier once the client confirms it with PUBREL.
     * The client entry itself is dropped when its last in-flight id is released,
     * so ids that never publish again do not linger for the lifetime of the JVM.
     *
     * @param clientId the client that published the packet
     * @param packetId the packet identifier to release
     */
    public void release(final String clientId, final int packetId) {
        inboundInflight.compute(clientId, (key, packetIds) -> {
            if (Objects.isNull(packetIds)) {
                return null;
            }
            packetIds.remove(packetId);
            return packetIds.isEmpty() ? null : packetIds;
        });
    }
}
