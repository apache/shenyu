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

package org.apache.shenyu.common.config.contract;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Optional, versioned configuration description; never a fleet revision or update action.
 * @param plugin stable plugin name
 * @param scope storage scope
 * @param version contract version
 * @param structuralValidation whether the declared structural constraints are enforced
 * @param unknownFields unknown field compatibility policy
 * @param fields declared fields and effective defaults
 * @param operations descriptions of missing/null/clear/update/remove behavior
 */
public record ConfigurationContract(String plugin, ConfigurationScope scope, String version, boolean structuralValidation,
                                    UnknownFields unknownFields, Map<String, Field> fields, Map<String, String> operations) {

    public ConfigurationContract {
        if (Objects.isNull(plugin) || !plugin.matches("[A-Za-z0-9_.-]{1,64}")
                || Objects.isNull(version) || !version.matches("[A-Za-z0-9_.-]{1,32}")) {
            throw new IllegalArgumentException("Invalid configuration contract identity");
        }
        Objects.requireNonNull(scope);
        Objects.requireNonNull(unknownFields);
        fields = Map.copyOf(fields);
        operations = Map.copyOf(operations);
        if (fields.size() > 256 || operations.size() > 16) {
            throw new IllegalArgumentException("Configuration contract metadata limit exceeded");
        }
        fields.keySet().forEach(ConfigurationViolation::checkPath);
    }

    /** Unknown fields remain a per-contract compatibility decision. */
    public enum UnknownFields {
        ALLOW, REJECT, SEMANTIC_VALIDATOR
    }

    /** JSON field types; a descriptor alone does not enable strict coercion rules. */
    public enum Type {
        STRING, INTEGER, NUMBER, BOOLEAN, OBJECT, ARRAY
    }

    /**
     * Field metadata and optional structural constraints. Defaults describe execution;
     * validation never inserts them into stored JSON.
     * @param type JSON type
     * @param required whether absence is invalid in structural mode
     * @param nullable whether explicit null is allowed in structural mode
     * @param effectiveDefault effective execution default, or null when conditional/unknown
     * @param unit field unit, or empty
     * @param sensitive whether tooling must omit supplied values
     * @param minimum optional inclusive numeric lower bound
     * @param maximum optional inclusive numeric upper bound
     * @param values optional closed string enumeration (never inferred for SPI names)
     */
    public record Field(Type type, boolean required, boolean nullable, String effectiveDefault, String unit,
                        boolean sensitive, Long minimum, Long maximum, List<String> values) {

        public Field {
            Objects.requireNonNull(type);
            unit = Objects.requireNonNullElse(unit, "");
            values = List.copyOf(values);
            if (values.size() > 64 || Objects.nonNull(minimum) && Objects.nonNull(maximum) && minimum > maximum) {
                throw new IllegalArgumentException("Invalid configuration field constraints");
            }
        }
    }
}
