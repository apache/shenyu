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

package org.apache.shenyu.plugin.agent.gateway.security;

import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentMcpIdentityTest {

    @Test
    void legacyAdaptersHaveNoFabricatedTenant() {
        AgentMcpIdentity identity = new AgentMcpIdentity("legacy", Set.of("read"));
        assertTrue(identity.getTenant().isEmpty());
        assertTrue(identity.getToolDenials().isEmpty());
        assertEquals(Set.of("read"), identity.getToolGrants());
    }

    @Test
    void denyWinsAndInputsAreRequestOwnedCopies() {
        Set<String> grants = new HashSet<>(Set.of("read", "write"));
        Set<String> denies = new HashSet<>(Set.of("write", "unknown"));
        AgentMcpIdentity identity = new AgentMcpIdentity("tenant", "subject", grants, denies);
        grants.add("admin");
        denies.clear();
        assertEquals(Set.of("read"), identity.getToolGrants());
        assertEquals(Set.of("write", "unknown"), identity.getToolDenials());
        assertEquals("tenant", identity.getTenant().orElseThrow());
        assertThrows(UnsupportedOperationException.class, () -> identity.getToolGrants().clear());
    }

    @Test
    void tenantBoundConstructorRequiresAnExplicitNonBlankTenant() {
        assertThrows(NullPointerException.class, () -> new AgentMcpIdentity(null, "subject", Set.of(), Set.of()));
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpIdentity(" ", "subject", Set.of(), Set.of()));
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpIdentity("tenant", " ", Set.of(), Set.of()));
    }
}
