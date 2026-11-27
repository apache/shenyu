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

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Locale;
import java.util.Objects;

/**
 * The base64 {@code data:} URL text carriers of a payload, for example the
 * {@code file_data} of an OpenAI file content part. Only a textual media type
 * is scannable, so {@code text/*} and {@code application/json} are decoded and
 * every other media type, a plain non data URL string and a corrupt payload are
 * reported as not scannable and left untouched. Decoding tolerates the two
 * base64 flavours real clients emit: MIME wrapped payloads with line breaks and
 * the URL safe alphabet, so a wrapped or URL safe {@code file_data} is scanned
 * instead of silently passed through.
 */
final class Base64TextData {

    private static final String DATA_SCHEME = "data:";

    private static final String BASE64_PARAMETER = ";base64";

    private Base64TextData() {
    }

    /**
     * The header of a textual base64 {@code data:} URL, up to and including the
     * comma that separates it from the payload.
     *
     * @param value the member value
     * @return the header, or null when the value is not a textual base64 data URL
     */
    static String prefix(final String value) {
        if (Objects.isNull(value) || !value.startsWith(DATA_SCHEME)) {
            return null;
        }
        int comma = value.indexOf(',');
        if (comma < 0) {
            return null;
        }
        String meta = value.substring(DATA_SCHEME.length(), comma).toLowerCase(Locale.ROOT);
        if (!meta.endsWith(BASE64_PARAMETER)) {
            return null;
        }
        int semicolon = meta.indexOf(';');
        String mediaType = semicolon < 0 ? meta : meta.substring(0, semicolon);
        return isTextualMediaType(mediaType) ? value.substring(0, comma + 1) : null;
    }

    /**
     * Whether the value is a textual base64 {@code data:} URL, so its payload is
     * text this plugin claims to scan.
     *
     * @param value the member value
     * @return true when the payload is scannable text
     */
    static boolean isTextualDataUrl(final String value) {
        return Objects.nonNull(prefix(value));
    }

    /**
     * Decode the payload of a textual base64 {@code data:} URL to text.
     *
     * @param value the member value
     * @return the decoded text, or null when the value is not a textual base64
     *         data URL or its payload is corrupt
     */
    static String decode(final String value) {
        String header = prefix(value);
        if (Objects.isNull(header)) {
            return null;
        }
        byte[] bytes = decodePayload(value.substring(header.length()));
        return Objects.isNull(bytes) ? null : new String(bytes, StandardCharsets.UTF_8);
    }

    /**
     * Encode text back under a data URL header, keeping the member a valid
     * base64 {@code data:} URL. The standard padded alphabet is used, so a MIME
     * wrapped or URL safe original is normalised by a redaction.
     *
     * @param prefix the data URL header of the original value
     * @param text the redacted text
     * @return the re encoded data URL
     */
    static String encode(final String prefix, final String text) {
        return prefix + Base64.getEncoder().encodeToString(text.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Whether the value is a textual base64 {@code data:} URL whose payload
     * cannot be decoded. Such a member is a malformed extraction shape: the text
     * is claimed to be scanned, so passing it through silently would leak it. A
     * binary media type is out of scope instead and is not malformed.
     *
     * @param value the member value
     * @return true when the payload is textual but undecodable
     */
    static boolean isUndecodableTextualData(final String value) {
        return isTextualDataUrl(value) && Objects.isNull(decode(value));
    }

    private static byte[] decodePayload(final String payload) {
        try {
            if (payload.indexOf('-') >= 0 || payload.indexOf('_') >= 0) {
                // the URL safe decoder is strict about separators, so drop the wrapping first
                return Base64.getUrlDecoder().decode(payload.replaceAll("\\s", ""));
            }
            // the MIME decoder tolerates line breaks and skips other non alphabet characters
            return Base64.getMimeDecoder().decode(payload);
        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    private static boolean isTextualMediaType(final String mediaType) {
        return mediaType.startsWith("text/") || "application/json".equals(mediaType);
    }
}
