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

package org.apache.shenyu.common.utils;

import java.util.Objects;

/** Exact local and namespaced MCP prompt identifiers shared by admission and grants. */
public final class AgentPromptNames {

    public static final int MAX_UPSTREAM_LENGTH = 128;

    public static final int MAX_EXPOSED_LENGTH = 177;

    private AgentPromptNames() {
    }

    /**
     * Check an original prompt name before adding the controlled server prefix.
     * @param name exact original name
     * @return whether the original name is valid
     */
    public static boolean original(final String name) {
        return Objects.nonNull(name) && name.matches("[A-Za-z0-9_.-]{1,128}");
    }

    /**
     * Check a local name or a server-prefixed original name.
     * @param name exact exposed name
     * @return whether the name can be granted or invoked
     */
    public static boolean exposed(final String name) {
        return original(name) || remote(name);
    }

    /**
     * Check the controlled server prefix and bounded original name independently.
     * @param name exposed remote name
     * @return whether the remote name is valid
     */
    public static boolean remote(final String name) {
        return Objects.nonNull(name) && name.matches("[A-Za-z0-9_-]{1,48}\\.[A-Za-z0-9_.-]{1,128}");
    }
}
