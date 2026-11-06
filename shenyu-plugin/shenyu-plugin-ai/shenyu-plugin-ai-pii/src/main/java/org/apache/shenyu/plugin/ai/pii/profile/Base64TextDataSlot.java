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

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.util.Objects;

/**
 * The slot of a textual base64 {@code data:} URL member, created by
 * {@link PiiTextSlot#ofBase64TextData}. The payload is decoded once and cached
 * for the lifetime of the slot, so a large payload is not decoded again per
 * read; a replacement re encodes the redacted text under the original data URL
 * header, keeping the member a valid base64 {@code data:} URL.
 */
final class Base64TextDataSlot implements PiiTextSlot {

    private final JsonObject parent;

    private final String member;

    private String cached;

    private boolean decoded;

    Base64TextDataSlot(final JsonObject parent, final String member) {
        this.parent = parent;
        this.member = member;
    }

    @Override
    public String text() {
        if (!decoded) {
            JsonElement current = parent.get(member);
            cached = Objects.nonNull(current) && current.isJsonPrimitive()
                    ? Base64TextData.decode(current.getAsString())
                    : null;
            decoded = true;
        }
        return cached;
    }

    @Override
    public void replace(final String newText) {
        JsonElement current = parent.get(member);
        if (Objects.nonNull(current) && current.isJsonPrimitive()) {
            String prefix = Base64TextData.prefix(current.getAsString());
            if (Objects.nonNull(prefix)) {
                parent.addProperty(member, Base64TextData.encode(prefix, newText));
            }
        }
        // keep the cache coherent with the tree for a later read of the same slot
        cached = newText;
        decoded = true;
    }
}
