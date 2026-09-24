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

package org.apache.shenyu.plugin.agent.gateway;

import java.util.Objects;

/**
 * Immutable request context for one agent traffic request.
 *
 * <p>The context is created per subscription and is never stored in a global
 * map or a thread local. This keeps concurrent agent requests isolated.</p>
 */
public final class AgentTrafficContext {

    private final String requestId;

    private final String trafficType;

    private final String selectorId;

    private final String ruleId;

    public AgentTrafficContext(final String requestId, final String trafficType,
                               final String selectorId, final String ruleId) {
        this.requestId = Objects.requireNonNull(requestId, "requestId");
        this.trafficType = Objects.requireNonNull(trafficType, "trafficType");
        this.selectorId = selectorId;
        this.ruleId = ruleId;
    }

    public String getRequestId() {
        return requestId;
    }

    public String getTrafficType() {
        return trafficType;
    }

    public String getSelectorId() {
        return selectorId;
    }

    public String getRuleId() {
        return ruleId;
    }
}
