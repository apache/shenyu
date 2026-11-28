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

package org.apache.shenyu.common.dto;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Creates read-only snapshots without changing the mutable source DTOs.
 * Only the standard DTO classes are supported, preserving their value equality.
 */
public final class DtoSnapshots {

    private DtoSnapshots() {
    }

    /**
     * Take a read-only plugin snapshot, reusing an existing snapshot.
     *
     * @param source source data, or null
     * @return a snapshot of the same concrete class, or null
     * @throws IllegalArgumentException if the source is a subclass
     */
    public static PluginData freeze(final PluginData source) {
        if (Objects.isNull(source)) {
            return null;
        }
        requireStandardClass(source, PluginData.class);
        if (source.frozen()) {
            return source;
        }
        PluginData snapshot = new PluginData(source);
        snapshot.markFrozen();
        return snapshot;
    }

    /**
     * Take a read-only selector snapshot, including current and previous conditions.
     *
     * @param source source data, or null
     * @return a snapshot of the same concrete class, or null
     * @throws IllegalArgumentException if the source or any condition is a subclass
     */
    public static SelectorData freeze(final SelectorData source) {
        if (Objects.isNull(source)) {
            return null;
        }
        requireStandardClass(source, SelectorData.class);
        if (source.frozen()) {
            return source;
        }
        SelectorData snapshot = new SelectorData(source);
        snapshot.markFrozen();
        return snapshot;
    }

    /**
     * Take a read-only rule snapshot, including current and previous conditions.
     *
     * @param source source data, or null
     * @return a snapshot of the same concrete class, or null
     * @throws IllegalArgumentException if the source or any condition is a subclass
     */
    public static RuleData freeze(final RuleData source) {
        if (Objects.isNull(source)) {
            return null;
        }
        requireStandardClass(source, RuleData.class);
        if (source.frozen()) {
            return source;
        }
        RuleData snapshot = new RuleData(source);
        snapshot.markFrozen();
        return snapshot;
    }

    /**
     * Take a read-only condition snapshot, reusing an existing snapshot.
     *
     * @param source source data, or null
     * @return a snapshot of the same concrete class, or null
     * @throws IllegalArgumentException if the source is a subclass
     */
    public static ConditionData freeze(final ConditionData source) {
        if (Objects.isNull(source)) {
            return null;
        }
        requireStandardClass(source, ConditionData.class);
        if (source.frozen()) {
            return source;
        }
        ConditionData snapshot = new ConditionData(source);
        snapshot.markFrozen();
        return snapshot;
    }

    /**
     * Copy a condition list, retaining null lists and null elements.
     *
     * @param source source conditions
     * @return read-only condition snapshots, or null
     */
    static List<ConditionData> freezeConditions(final List<ConditionData> source) {
        if (Objects.isNull(source)) {
            return null;
        }
        List<ConditionData> snapshots = new ArrayList<>(source.size());
        for (ConditionData condition : source) {
            snapshots.add(freeze(condition));
        }
        return Collections.unmodifiableList(snapshots);
    }

    private static void requireStandardClass(final Object source, final Class<?> expected) {
        if (source.getClass() != expected) {
            throw new IllegalArgumentException("Unsupported DTO snapshot class: " + source.getClass().getName());
        }
    }
}
