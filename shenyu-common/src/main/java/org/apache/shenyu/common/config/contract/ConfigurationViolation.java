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

import java.util.Objects;

/**
 * Bounded diagnostics contain declared paths and stable codes, never supplied values or parser messages.
 * @param path declared field path or root
 * @param code stable reason
 */
public record ConfigurationViolation(String path, Code code) {

    public ConfigurationViolation {
        checkPath(path);
        Objects.requireNonNull(code);
    }

    static void checkPath(final String path) {
        if (Objects.isNull(path) || !path.matches("[A-Za-z0-9_.$\\[\\]-]{1,128}")) {
            throw new IllegalArgumentException("Invalid configuration diagnostic path");
        }
    }

    /** Shared structural/semantic errors and separately classified environment binding errors. */
    public enum Code {
        INVALID_JSON, REQUIRED, NULL_NOT_ALLOWED, INVALID_TYPE, OUT_OF_RANGE, INVALID_ENUM,
        UNKNOWN_FIELD, SEMANTIC_INVALID, RUNTIME_BINDING_UNAVAILABLE, PROVIDER_FAILURE
    }
}
