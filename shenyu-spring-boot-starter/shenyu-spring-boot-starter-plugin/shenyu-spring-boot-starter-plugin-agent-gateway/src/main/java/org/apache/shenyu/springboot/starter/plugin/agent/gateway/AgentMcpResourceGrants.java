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

package org.apache.shenyu.springboot.starter.plugin.agent.gateway;

import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceDefinition;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * Bounded exact resource permission snapshots shared by the built-in security adapters.
 */
final class AgentMcpResourceGrants {

    private AgentMcpResourceGrants() {
    }

    static Set<String> parse(final Object value) {
        if (!(value instanceof Collection<?> values) || values.size() > 1024) {
            throw new IllegalArgumentException("Resource grants must be a bounded collection");
        }
        Set<String> result = new HashSet<>();
        for (Object item : values) {
            if (!(item instanceof String name) || !name.equals(name.trim()) || "*".equals(name)) {
                throw new IllegalArgumentException("Resource grants require exact names");
            }
            // Reuse local and namespaced remote definition name validation, not tool-name syntax.
            AgentResourceDefinition.validateName(name);
            result.add(name);
        }
        return Set.copyOf(result);
    }
}
