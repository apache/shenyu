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

package org.apache.shenyu.web.diagnostics;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.common.utils.GsonUtils;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * The shared allowlist and redaction policy applied before anything enters a bundle.
 *
 * <p>Redaction is deliberately conservative: it is safer to hide a non-secret value than to
 * leak a credential. Secret material is recognised by key name and by a small set of value
 * shapes (bearer tokens, JWTs and {@code key=value} assignments). Free-form text and JSON are
 * both supported; unparseable JSON falls back to text scrubbing so a malformed payload can
 * never bypass the policy. Where a value must be correlatable but not revealed, use
 * {@link #digest(String)} to publish a stable SHA-256 fingerprint instead of the raw value.</p>
 */
public final class DiagnosticRedactor {

    /**
     * The replacement for any redacted value.
     */
    public static final String REDACTED = "******";

    private static final String SHA_256 = "SHA-256";

    private static final Set<String> SECRET_KEY_TOKENS = Set.of(
            "password", "passwd", "pwd", "secret", "token", "apikey", "accesskey",
            "secretkey", "privatekey", "credential", "authorization", "cookie",
            "bearer", "signature", "jwt", "sessionid");

    private static final Pattern SECRET_ASSIGN = Pattern.compile(
            "(?i)(\\b(?:password|passwd|pwd|secret|token|api[-_]?key|access[-_]?key|secret[-_]?key|"
                    + "private[-_]?key|credential|authorization|cookie|session[-_]?id)\\b\\s*[:=]\\s*)(\"?)([^\",}\\s]+)");

    private static final Pattern BEARER = Pattern.compile("(?i)(bearer\\s+)([A-Za-z0-9._~+/=-]+)");

    private static final Pattern JWT = Pattern.compile("\\beyJ[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+");

    /**
     * Whether a configuration key names secret material.
     *
     * @param key the key to test
     * @return true if the key must have its value redacted
     */
    public boolean isSecretKey(final String key) {
        if (Objects.isNull(key)) {
            return false;
        }
        String normalized = key.toLowerCase().replaceAll("[^a-z0-9]", "");
        return SECRET_KEY_TOKENS.stream().anyMatch(normalized::contains);
    }

    /**
     * Redact a JSON document, hiding the value of every secret-named member at any depth.
     *
     * <p>Unparseable input is scrubbed as free text so a malformed payload cannot bypass the
     * policy.</p>
     *
     * @param json the JSON document
     * @return the redacted JSON, or redacted text when the input is not valid JSON
     */
    public String redactJson(final String json) {
        if (Objects.isNull(json) || json.isEmpty()) {
            return json;
        }
        try {
            JsonElement root = JsonParser.parseString(json);
            JsonElement redacted = redactElement(root);
            return GsonUtils.getGson().toJson(redacted);
        } catch (RuntimeException ex) {
            return redactText(json);
        }
    }

    /**
     * Scrub secret value shapes from free-form text such as thread dumps or error messages.
     *
     * @param text the text to scrub
     * @return the scrubbed text
     */
    public String redactText(final String text) {
        if (Objects.isNull(text) || text.isEmpty()) {
            return text;
        }
        String result = BEARER.matcher(text).replaceAll("$1" + REDACTED);
        result = SECRET_ASSIGN.matcher(result).replaceAll("$1$2" + REDACTED);
        return JWT.matcher(result).replaceAll(REDACTED);
    }

    /**
     * Reduce a URL to its non-sensitive parts by dropping credentials and the query string.
     *
     * @param url the url to strip
     * @return the stripped url
     */
    public String stripUrl(final String url) {
        if (Objects.isNull(url) || url.isEmpty()) {
            return url;
        }
        String result = url.replaceAll("(?i)(://)[^/@?#]+@", "$1" + REDACTED + "@");
        int query = result.indexOf('?');
        if (query >= 0) {
            result = result.substring(0, query) + "?" + REDACTED;
        }
        int fragment = result.indexOf('#');
        if (fragment >= 0) {
            result = result.substring(0, fragment);
        }
        return result;
    }

    /**
     * Produce a stable, non-reversible SHA-256 fingerprint of a value.
     *
     * <p>Use this to keep configuration correlatable across bundles without ever publishing
     * the raw value. A {@code null} or empty value yields an empty fingerprint so callers do
     * not mistake "no value" for a real digest.</p>
     *
     * @param value the value to fingerprint
     * @return the lowercase hex digest, or an empty string for null/empty input
     */
    public String digest(final String value) {
        if (Objects.isNull(value) || value.isEmpty()) {
            return "";
        }
        try {
            MessageDigest messageDigest = MessageDigest.getInstance(SHA_256);
            byte[] hash = messageDigest.digest(value.getBytes(StandardCharsets.UTF_8));
            return HexFormat.of().formatHex(hash);
        } catch (NoSuchAlgorithmException ex) {
            return REDACTED;
        }
    }

    private JsonElement redactElement(final JsonElement element) {
        if (element.isJsonObject()) {
            return redactObject(element.getAsJsonObject());
        }
        if (element.isJsonArray()) {
            return redactArray(element.getAsJsonArray());
        }
        return element;
    }

    private JsonElement redactObject(final JsonObject source) {
        JsonObject target = new JsonObject();
        for (Map.Entry<String, JsonElement> entry : source.entrySet()) {
            if (isSecretKey(entry.getKey())) {
                target.addProperty(entry.getKey(), REDACTED);
            } else {
                target.add(entry.getKey(), redactElement(entry.getValue()));
            }
        }
        return target;
    }

    private JsonElement redactArray(final JsonArray source) {
        List<JsonElement> items = new ArrayList<>(source.size());
        for (JsonElement item : source) {
            items.add(redactElement(item));
        }
        JsonArray target = new JsonArray();
        items.forEach(target::add);
        return target;
    }
}
