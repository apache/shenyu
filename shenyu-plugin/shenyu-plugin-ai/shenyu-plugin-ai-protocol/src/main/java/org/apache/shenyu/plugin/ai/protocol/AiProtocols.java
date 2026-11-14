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

package org.apache.shenyu.plugin.ai.protocol;

/**
 * The single source of truth of the AI protocol identifiers used by the AI plugins.
 */
public final class AiProtocols {

    /**
     * The OpenAI Chat Completions protocol identifier.
     */
    public static final String OPENAI_CHAT = "openai-chat";

    /**
     * The OpenAI Responses protocol identifier.
     */
    public static final String OPENAI_RESPONSES = "openai-responses";

    /**
     * Sentinel for a request whose protocol stayed unresolved. It is never a valid
     * protocol identifier; callers use it when a later consumer must not derive a
     * best-effort hint and contradict the resolution.
     */
    public static final String UNKNOWN = "";

    private AiProtocols() {
    }
}
