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

package org.apache.shenyu.plugin.shadow.compare;

import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;
import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;
import org.springframework.http.HttpHeaders;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Optional;

/**
 * Compares a primary response with its shadow response.
 *
 * <p>The comparison is bounded and value-free: the result names what differed
 * but never copies response bodies or header values into the report.</p>
 */
public final class ShadowComparator {

    private static final String CONTENT_TYPE = "Content-Type";

    private static final String JSON = "json";

    private ShadowComparator() {
    }

    /**
     * Compare the primary observation with the shadow observation.
     *
     * @param handle the validated rule handle
     * @param primary the primary response observation
     * @param shadow the shadow response observation
     * @return the comparison result
     */
    public static ShadowComparison compare(final ShadowHandle handle, final ShadowObservation primary,
                                           final ShadowObservation shadow) {
        if (primary.isTruncated() || shadow.isTruncated()) {
            return ShadowComparison.truncated("response body exceeded the comparison bound");
        }
        if (primary.getStatus() != shadow.getStatus()) {
            return ShadowComparison.mismatch("status " + primary.getStatus() + " != " + shadow.getStatus());
        }
        Optional<String> headerDifference = headerDifference(handle, primary.getHeaders(), shadow.getHeaders());
        if (headerDifference.isPresent()) {
            return ShadowComparison.mismatch(headerDifference.get());
        }
        return bodyDifference(handle, primary, shadow);
    }

    private static Optional<String> headerDifference(final ShadowHandle handle, final HttpHeaders primary,
                                                     final HttpHeaders shadow) {
        for (String name : handle.getCompareHeaders()) {
            List<String> left = primary.get(name);
            List<String> right = shadow.get(name);
            if (!Objects.equals(left, right)) {
                return Optional.of("header " + name + " differs");
            }
        }
        return Optional.empty();
    }

    private static ShadowComparison bodyDifference(final ShadowHandle handle, final ShadowObservation primary,
                                                   final ShadowObservation shadow) {
        boolean primaryJson = isJson(primary.getHeaders());
        boolean shadowJson = isJson(shadow.getHeaders());
        if (primaryJson != shadowJson) {
            return ShadowComparison.mismatch("content type differs");
        }
        if (!primaryJson) {
            return Arrays.equals(primary.getBody(), shadow.getBody())
                    ? ShadowComparison.match() : ShadowComparison.mismatch("body bytes differ");
        }
        return jsonDifference(handle, primary.getBody(), shadow.getBody());
    }

    private static ShadowComparison jsonDifference(final ShadowHandle handle, final byte[] primaryBody,
                                                   final byte[] shadowBody) {
        if (primaryBody.length == 0 && shadowBody.length == 0) {
            return ShadowComparison.match();
        }
        final JsonElement primaryTree;
        final JsonElement shadowTree;
        try {
            primaryTree = JsonParser.parseString(new String(primaryBody, StandardCharsets.UTF_8));
            shadowTree = JsonParser.parseString(new String(shadowBody, StandardCharsets.UTF_8));
        } catch (JsonSyntaxException e) {
            return ShadowComparison.mismatch("response body is not valid JSON");
        }
        if (primaryTree.equals(shadowTree)) {
            return ShadowComparison.match();
        }
        JsonElement normalizedPrimary = JsonPaths.normalized(primaryTree, handle.getIgnoreJsonPaths());
        JsonElement normalizedShadow = JsonPaths.normalized(shadowTree, handle.getIgnoreJsonPaths());
        if (normalizedPrimary.equals(normalizedShadow)) {
            return ShadowComparison.normalized("difference only in ignored paths");
        }
        String path = JsonPaths.firstDifference(normalizedPrimary, normalizedShadow, "$").orElse("$");
        return ShadowComparison.mismatch("body path " + path + " differs");
    }

    private static boolean isJson(final HttpHeaders headers) {
        String contentType = headers.getFirst(CONTENT_TYPE);
        return Objects.nonNull(contentType) && contentType.toLowerCase(Locale.ROOT).contains(JSON);
    }
}
