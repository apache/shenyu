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

/**
 * Tracks the inbound packet identifiers currently being acknowledged, so that
 * a retransmitted (DUP) publish can be answered without re-delivering it (#7919).
 * Entries are released by PUBREL; a clean-session disconnect discards them with
 * the rest of the session state.
 */
public class InflightRepository implements BaseRepository<String, Set<Integer>> {

    private static final Map<String, Set<Integer>> INBOUND_INFLIGHT = new ConcurrentHashMap<>();

    @Override
    public void add(final String clientId, final Set<Integer> packetIds) {
        INBOUND_INFLIGHT.put(clientId, packetIds);
    }

    @Override
    public void remove(final String clientId) {
        INBOUND_INFLIGHT.remove(clientId);
    }

    @Override
    public Set<Integer> get(final String clientId) {
        return INBOUND_INFLIGHT.get(clientId);
    }

    /**
     * Registers a packet identifier as being acknowledged.
     *
     * @param clientId the client that published the packet
     * @param packetId the packet identifier of the inbound publish
     * @return true when the packet was seen for the first time, false on a retransmission
     */
    public boolean register(final String clientId, final int packetId) {
        return INBOUND_INFLIGHT.computeIfAbsent(clientId, key -> ConcurrentHashMap.newKeySet()).add(packetId);
    }

    /**
     * Releases the packet identifier once the client confirms it with PUBREL.
     *
     * @param clientId the client that published the packet
     * @param packetId the packet identifier to release
     */
    public void release(final String clientId, final int packetId) {
        Set<Integer> packetIds = INBOUND_INFLIGHT.get(clientId);
        if (Objects.nonNull(packetIds)) {
            packetIds.remove(packetId);
        }
    }
}
