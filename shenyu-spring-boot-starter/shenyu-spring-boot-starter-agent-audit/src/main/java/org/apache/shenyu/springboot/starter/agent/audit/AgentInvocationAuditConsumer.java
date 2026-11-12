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

package org.apache.shenyu.springboot.starter.agent.audit;

import org.apache.shenyu.common.audit.AgentInvocationAuditEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * Default diagnostic sink. A deployment can replace it without changing agent traffic boundaries.
 */
@FunctionalInterface
public interface AgentInvocationAuditConsumer {

    AgentInvocationAuditConsumer LOGGING = AgentInvocationAuditConsumer::log;

    void accept(List<AgentInvocationAuditEvent> events);

    private static void log(final List<AgentInvocationAuditEvent> events) {
        Logger logger = LoggerFactory.getLogger("org.apache.shenyu.agent.invocation.audit");
        events.forEach(event -> logger.info("schema={} type={} request={} tenant={} subject={} apiKeyId={} selector={} rule={} "
                        + "configuration={} policy={} target={} decision={} outcome={} reason={} status={} attempt={} elapsedMs={} timestamp={}",
                event.getSchemaVersion(), event.getRecordType(), event.getRequestId(), event.getTenant().orElse(null),
                event.getSubject().orElse(null), event.getApiKeyId().orElse(null), event.getSelectorId().orElse(null),
                event.getRuleId().orElse(null), event.getConfigurationVersion(), event.getPolicyVersion(), event.getTarget().orElse(null),
                event.getDecision(), event.getOutcome(), event.getReason(), event.getHttpStatus().orElse(null), event.getAttempt().orElse(null),
                event.getElapsedMillis(), event.getTimestamp()));
    }
}
