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

package org.apache.shenyu.plugin.ai.pii;

import java.util.Objects;
/**
 * The action taken when a detector matches, ordered from the strictest to the
 * least strict. When detections of different types overlap on the same text
 * range, the strictest action wins, so a block is never silently downgraded to
 * a redaction or an audit record by a concurrent weaker detection.
 */

public enum PiiAction {

    /**
     * The request or response is rejected, protected content never reaches the
     * other side.
     */
    BLOCK,

    /**
     * The matched range is replaced by a type placeholder, the rest of the
     * payload is forwarded unchanged.
     */
    REDACT,

    /**
     * The payload is forwarded unchanged, the detection is only recorded
     * without the detected value.
     */
    AUDIT;

    /**
     * Resolve a configured action name.
     *
     * @param name the configured name, case insensitive
     * @return the action, or null when the name is not a known action
     */
    public static PiiAction fromName(final String name) {
        if (Objects.isNull(name) || name.trim().isEmpty()) {
            return null;
        }
        for (PiiAction action : values()) {
            if (action.name().equalsIgnoreCase(name.trim())) {
                return action;
            }
        }
        return null;
    }
}
