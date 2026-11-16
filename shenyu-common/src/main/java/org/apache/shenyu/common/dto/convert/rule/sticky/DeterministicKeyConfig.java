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

package org.apache.shenyu.common.dto.convert.rule.sticky;

import java.util.Objects;

/**
 * Deterministic key extraction for hash-based selection (#8037).
 *
 * <p>Absent configuration keeps the plain load-balancing behavior: no key is
 * extracted and no request data is read. Deterministic hashing is not an
 * explicit binding — it writes no cookie of its own, while an active sticky
 * binding still records the chosen target for new sessions — and it re-maps
 * whenever the candidate set changes; it maps a key to the same target only
 * while configuration, identities and candidate set are equivalent across
 * gateway replicas.
 */
public class DeterministicKeyConfig {

    /**
     * Where the deterministic key comes from.
     */
    public enum KeyType {

        /**
         * A single request header, addressed by name.
         */
        HEADER,

        /**
         * A single request cookie, addressed by name.
         */
        COOKIE
    }

    /**
     * Default source when a key is configured without an explicit type.
     */
    public static final KeyType DEFAULT_TYPE = KeyType.HEADER;

    /**
     * Where the key is read from.
     */
    private KeyType type = DEFAULT_TYPE;

    /**
     * The header or cookie name the key is read from.
     */
    private String name;

    /**
     * Get the key source.
     *
     * @return the key source
     */
    public KeyType getType() {
        return type;
    }

    /**
     * Set the key source.
     *
     * @param type the key source
     */
    public void setType(final KeyType type) {
        this.type = type;
    }

    /**
     * Get the header or cookie name.
     *
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * Set the header or cookie name.
     *
     * @param name the name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * Whether a usable key source is configured.
     *
     * @return true when a non-blank name is configured
     */
    public boolean isConfigured() {
        return Objects.nonNull(name) && !name.isBlank();
    }
}
