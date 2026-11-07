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

/** Bounded shared validation or runtime-binding failure without input values or parser causes. */
public final class ConfigurationValidationException extends IllegalArgumentException {

    private static final long serialVersionUID = 1L;

    private final ConfigurationValidation validation;

    public ConfigurationValidationException(final ConfigurationValidation validation) {
        super("Invalid configuration: " + validation.plugin() + " " + validation.scope() + " "
                + validation.violations().get(0).path() + " " + validation.violations().get(0).code());
        this.validation = validation;
    }

    /**
     * Get the shared, secret-free failure facts.
     * @return bounded diagnostic result
     */
    public ConfigurationValidation validation() {
        return validation;
    }

    /**
     * Classify an environment-specific extension binding failure separately from configuration semantics.
     * @param plugin trusted plugin identity
     * @param scope scope
     * @param path declared field path
     * @return bounded binding failure with no raw provider exception
     */
    public static ConfigurationValidationException bindingUnavailable(final String plugin, final ConfigurationScope scope, final String path) {
        return new ConfigurationValidationException(new ConfigurationValidation(plugin, scope, "1", ConfigurationValidation.Status.INVALID,
                List.of(new ConfigurationViolation(path, ConfigurationViolation.Code.RUNTIME_BINDING_UNAVAILABLE))));
    }
}
