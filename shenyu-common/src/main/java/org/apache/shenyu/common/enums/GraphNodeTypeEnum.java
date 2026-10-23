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

package org.apache.shenyu.common.enums;

import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;

/**
 * The type of a node in a plugin orchestration graph.
 *
 * <p>See {@code docs/plugin-graph-orchestration.md} (issue #7994) for the node semantics and
 * {@code docs/plugin-graph-compatibility-audit.md} for the placement rules derived per plugin.</p>
 *
 * @since 2.7.2
 */
public enum GraphNodeTypeEnum {

    /**
     * Binds one installed plugin by name and has exactly one successor.
     */
    PLUGIN("PLUGIN"),

    /**
     * Evaluates ordered branches, first match wins, with a mandatory default target.
     */
    CHOICE("CHOICE"),

    /**
     * Terminal node; completes the traversal.
     */
    END("END");

    private final String name;

    /**
     * all args constructor.
     *
     * @param name the canonical name persisted in {@code GraphNodeData#type}
     */
    GraphNodeTypeEnum(final String name) {
        this.name = name;
    }

    /**
     * get name.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * Resolve a graph node type from its canonical name.
     *
     * @param name the persisted type name
     * @return the matching enum, or empty when the name is null or unknown
     */
    public static Optional<GraphNodeTypeEnum> fromName(final String name) {
        if (Objects.isNull(name)) {
            return Optional.empty();
        }
        return Arrays.stream(values())
                .filter(type -> type.name.equals(name))
                .findFirst();
    }

    /**
     * Whether the given name is a canonical graph node type name.
     *
     * @param name the persisted type name
     * @return true when the name maps to a known type
     */
    public static boolean isValidName(final String name) {
        return fromName(name).isPresent();
    }
}
