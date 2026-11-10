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

/**
 * Lightweight shared provider. Implementations must not initialize gateway plugins or external clients.
 * Register through the existing application wiring or Java ServiceLoader at startup.
 */
public interface ConfigurationContractProvider {

    /**
     * Describe the provider's one immutable contract.
     * @return contract metadata
     */
    ConfigurationContract contract();

    /**
     * Run pure plugin-owned semantics after optional structural validation.
     * @param source original JSON, including missing/blank input
     * @return immutable-compatible diagnostics, never supplied values or exception text
     */
    default List<ConfigurationViolation> validateSemantic(final String source) {
        return List.of();
    }
}
