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

package org.apache.shenyu.common.governance;

import java.util.Objects;

/** Shared exact-text validation; invalid identities are never silently normalized. */
public final class AgentGovernanceText {

    private AgentGovernanceText() {
    }

    /**
     * Check text against a field-specific bound.
     * @param value exact field value
     * @param maximum maximum characters
     * @return whether the field is valid
     */
    public static boolean valid(final String value, final int maximum) {
        return Objects.nonNull(value) && !value.isBlank() && value.length() <= maximum
                && value.equals(value.strip()) && value.chars().noneMatch(Character::isISOControl);
    }

    /**
     * Require exact bounded text.
     * @param value field value
     * @param name safe static field name
     * @param maximum maximum characters
     * @return original valid value
     */
    public static String require(final String value, final String name, final int maximum) {
        if (!valid(value, maximum)) {
            throw new IllegalArgumentException(name + " must be bounded nonblank text");
        }
        return value;
    }
}
