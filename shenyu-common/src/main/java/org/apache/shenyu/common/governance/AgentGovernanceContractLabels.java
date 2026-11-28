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

package org.apache.shenyu.common.governance;

import java.util.Objects;

/**
 * The only bounded labels exposed to metrics. Identity and correlation values stay
 * in the decision and its audit adapter rather than becoming metric cardinality.
 */
public final class AgentGovernanceContractLabels {

    private final String consumer;

    private final String mode;

    private final String result;

    private final String reason;

    private AgentGovernanceContractLabels(final AgentGovernanceDecision decision) {
        Objects.requireNonNull(decision, "decision");
        consumer = decision.getConsumer().name();
        mode = decision.getMode().name();
        result = decision.getResult().name();
        reason = decision.getReason().name();
    }

    public static AgentGovernanceContractLabels from(final AgentGovernanceDecision decision) {
        return new AgentGovernanceContractLabels(decision);
    }

    public String getConsumer() {
        return consumer;
    }

    public String getMode() {
        return mode;
    }

    public String getResult() {
        return result;
    }

    public String getReason() {
        return reason;
    }
}
