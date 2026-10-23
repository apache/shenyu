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

package org.apache.shenyu.common.graph;

import org.apache.shenyu.common.exception.ShenyuException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Thrown when a plugin orchestration graph fails validation.
 *
 * <p>Carries the full, ordered list of violations so callers can surface every actionable error at
 * once (naming the offending node id / plugin name) rather than only the first. Validation is
 * fail-closed: a graph that cannot be validated is never compiled or activated.</p>
 *
 * @since 2.7.2
 */
public class GraphValidationException extends ShenyuException {

    private static final long serialVersionUID = -6871045473042167831L;

    private final List<String> violations;

    /**
     * Instantiates a new GraphValidationException.
     *
     * @param violations the ordered, non-empty list of violation messages
     */
    public GraphValidationException(final List<String> violations) {
        super(buildMessage(violations));
        this.violations = Collections.unmodifiableList(new ArrayList<>(Objects.requireNonNull(violations, "violations")));
    }

    /**
     * The ordered, unmodifiable list of violation messages.
     *
     * @return the violations
     */
    public List<String> getViolations() {
        return violations;
    }

    private static String buildMessage(final List<String> violations) {
        if (Objects.isNull(violations) || violations.isEmpty()) {
            return "plugin graph validation failed";
        }
        return "plugin graph validation failed with " + violations.size() + " violation(s): " + String.join("; ", violations);
    }
}
