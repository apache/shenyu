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

package org.apache.shenyu.plugin.agent.gateway.resource;

import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolInvocation;

import java.util.Map;
import java.util.Objects;

/**
 * Resource input bound to the same trusted context as a tool call.
 */
public final class AgentResourceInvocation {

    private final AgentToolInvocation context;

    private final String uri;

    private final Map<String, String> variables;

    public AgentResourceInvocation(final AgentToolInvocation context, final String uri, final Map<String, String> variables) {
        this.context = Objects.requireNonNull(context, "context");
        this.uri = Objects.requireNonNull(uri, "uri");
        this.variables = Map.copyOf(variables);
    }

    public AgentToolInvocation getContext() {
        return context;
    }

    public String getUri() {
        return uri;
    }

    public Map<String, String> getVariables() {
        return variables;
    }
}
