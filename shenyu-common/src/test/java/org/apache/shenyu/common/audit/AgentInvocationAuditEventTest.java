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

package org.apache.shenyu.common.audit;

import org.junit.jupiter.api.Test;

import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AgentInvocationAuditEventTest {

    @Test
    void buildsVersionedLogicalRecordWithoutCredentialCarryingFields() {
        AgentInvocationAuditEvent event = AgentInvocationAuditEvent.logical()
                .requestId("request-1")
                .tenant("tenant-1")
                .subject("subject-1")
                .apiKeyId("internal-key-id")
                .selectorId("selector-1")
                .ruleId("rule-1")
                .configurationVersion("configuration-1")
                .policyVersion("policy-1")
                .target("tool-1")
                .outcome("SUCCESS")
                .reason("COMPLETED")
                .httpStatus(200)
                .elapsedMillis(1)
                .timestamp(Instant.EPOCH)
                .build();

        assertEquals(AgentInvocationAuditEvent.SCHEMA_VERSION, event.getSchemaVersion());
        assertEquals(AgentInvocationAuditEvent.RecordType.LOGICAL, event.getRecordType());
        assertEquals(AgentInvocationAuditEvent.Decision.ALLOWED, event.getDecision());
        assertEquals("internal-key-id", event.getApiKeyId().orElseThrow());
        assertEquals(Instant.EPOCH, event.getTimestamp());
    }

    @Test
    void rejectsUnboundedOrMalformedAuditTextAndInvalidAttempts() {
        assertThrows(IllegalArgumentException.class, () -> AgentInvocationAuditEvent.logical()
                .requestId(" request ").outcome("SUCCESS").build());
        assertThrows(IllegalArgumentException.class, () -> AgentInvocationAuditEvent.logical()
                .requestId("x".repeat(257)).outcome("SUCCESS").build());
        assertThrows(IllegalArgumentException.class, () -> AgentInvocationAuditEvent.attempt(0)
                .requestId("request").outcome("FAILED").build());
    }
}
