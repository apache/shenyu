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

package org.apache.shenyu.plugin.agent.gateway.prompt;

import java.time.Instant;
import java.util.Map;
import java.util.Objects;

/** Immutable trusted identity and request-owned prompt arguments. */
public record AgentPromptInvocation(String requestId, String tenant, String subject, String ruleId, long configurationVersion,
                                    Instant deadline, Map<String, String> arguments) {
    public AgentPromptInvocation {
        Objects.requireNonNull(requestId, "requestId");
        Objects.requireNonNull(subject, "subject");
        Objects.requireNonNull(ruleId, "ruleId");
        Objects.requireNonNull(deadline, "deadline");
        arguments = Map.copyOf(arguments);
    }
}
