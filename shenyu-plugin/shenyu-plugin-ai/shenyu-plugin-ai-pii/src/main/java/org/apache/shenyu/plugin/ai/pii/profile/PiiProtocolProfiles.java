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

package org.apache.shenyu.plugin.ai.pii.profile;

import com.google.gson.JsonObject;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * The known protocol profiles, selected by the shape of the payload, with the
 * same discrimination the AI proxy uses: a {@code messages} member names a
 * chat completions payload, an {@code input} member names a responses
 * payload. A payload no profile recognizes is an unsupported enforcement
 * mode, the caller applies the failure policy of the rule.
 */
public final class PiiProtocolProfiles {

    private static final List<PiiProtocolProfile> PROFILES = Arrays.asList(
            new OpenAiChatProfile(), new OpenAiResponsesProfile());

    private PiiProtocolProfiles() {
    }

    /**
     * Select the profile of a request payload by its shape.
     *
     * @param root the parsed payload
     * @return the matching profile, or null when no known protocol matches
     */
    public static PiiProtocolProfile select(final JsonObject root) {
        if (Objects.isNull(root)) {
            return null;
        }
        for (PiiProtocolProfile profile : PROFILES) {
            if (matchesRequest(profile, root)) {
                return profile;
            }
        }
        return null;
    }

    /**
     * Select the profile of a response payload by its shape: a chat
     * completions response is recognized by {@code choices}, a responses
     * payload by {@code output}, the shapes of the two directions differ.
     *
     * @param root the parsed payload
     * @return the matching profile, or null when no known protocol matches
     */
    public static PiiProtocolProfile selectResponse(final JsonObject root) {
        if (Objects.isNull(root)) {
            return null;
        }
        for (PiiProtocolProfile profile : PROFILES) {
            if (matchesResponse(profile, root)) {
                return profile;
            }
        }
        return null;
    }

    private static boolean matchesRequest(final PiiProtocolProfile profile, final JsonObject root) {
        if (OpenAiChatProfile.ID.equals(profile.id())) {
            return root.has("messages");
        }
        if (OpenAiResponsesProfile.ID.equals(profile.id())) {
            return root.has("input") && !root.has("messages");
        }
        return false;
    }

    private static boolean matchesResponse(final PiiProtocolProfile profile, final JsonObject root) {
        if (OpenAiChatProfile.ID.equals(profile.id())) {
            return root.has("choices");
        }
        if (OpenAiResponsesProfile.ID.equals(profile.id())) {
            return root.has("output");
        }
        return false;
    }
}
