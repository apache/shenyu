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

package org.apache.shenyu.plugin.agent.gateway.resource;

import com.google.gson.JsonObject;

import java.net.URI;
import java.util.Base64;
import java.util.Objects;
import java.util.regex.Pattern;

/**
 * Immutable resource contents, independent of tool media or completion extensions.
 * Resource identifiers are metadata and are never dereferenced.
 */
public final class AgentResourceContent {

    private static final Pattern MIME = Pattern.compile("[a-zA-Z0-9!#$&^_.+-]+/[a-zA-Z0-9!#$&^_.+-]+");

    private final JsonObject value;

    private AgentResourceContent(final String uri, final String mimeType, final String field, final String body) {
        URI identifier = URI.create(Objects.requireNonNull(uri, "uri"));
        if (!identifier.isAbsolute() || Objects.nonNull(identifier.getRawUserInfo())
                || uri.codePoints().anyMatch(character -> Character.isWhitespace(character) || Character.isISOControl(character))) {
            throw new IllegalArgumentException("Resource URI must be absolute and contain no credentials");
        }
        value = new JsonObject();
        value.addProperty("uri", uri);
        if (Objects.nonNull(mimeType)) {
            if (!MIME.matcher(mimeType).matches()) {
                throw new IllegalArgumentException("Invalid MIME type");
            }
            value.addProperty("mimeType", mimeType);
        }
        value.addProperty(field, Objects.requireNonNull(body, field));
    }

    /**
     * Create resource text without fetching the URI.
     * @param uri absolute resource identifier
     * @param mimeType optional MIME type
     * @param text resource text, including empty text
     * @return immutable resource content
     */
    public static AgentResourceContent text(final String uri, final String mimeType, final String text) {
        return new AgentResourceContent(uri, mimeType, "text", text);
    }

    /**
     * Create binary resource contents without fetching the URI.
     * @param uri absolute resource identifier
     * @param mimeType optional MIME type
     * @param blob canonical base64, including an empty binary resource
     * @return immutable resource content
     */
    public static AgentResourceContent blob(final String uri, final String mimeType, final String blob) {
        if (!Base64.getEncoder().encodeToString(Base64.getDecoder().decode(Objects.requireNonNull(blob, "blob"))).equals(blob)) {
            throw new IllegalArgumentException("Resource blob must use canonical base64");
        }
        return new AgentResourceContent(uri, mimeType, "blob", blob);
    }

    /**
     * Return an independent resource wire snapshot.
     * @return resource contents
     */
    public JsonObject toJson() {
        return value.deepCopy();
    }
}
