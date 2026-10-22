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

package org.apache.shenyu.plugin.cache;

import com.google.gson.Gson;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.server.reactive.ServerHttpResponse;
import reactor.core.publisher.Mono;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

/**
 * A complete response stored atomically as metadata followed by unencoded body bytes.
 */
final class CachedResponse {

    private static final Gson GSON = new Gson();

    private static final Pattern HEADER_NAME = Pattern.compile("[!#$%&'*+.^_`|~0-9A-Za-z-]+");

    private static final Set<String> HOP_BY_HOP = Set.of(HttpHeaders.CONNECTION, "Keep-Alive", HttpHeaders.PROXY_AUTHENTICATE,
            HttpHeaders.PROXY_AUTHORIZATION, HttpHeaders.TE, HttpHeaders.TRAILER, HttpHeaders.TRANSFER_ENCODING, HttpHeaders.UPGRADE);

    private final Metadata metadata;

    private final byte[] body;

    private CachedResponse(final Metadata metadata, final byte[] body) {
        this.metadata = metadata;
        this.body = body;
    }

    static Optional<CachedResponse> capture(final ServerHttpResponse response, final byte[] body) {
        int status = Objects.requireNonNullElse(response.getStatusCode(), HttpStatus.OK).value();
        HttpHeaders headers = response.getHeaders();
        if (status < 200 || status == 206 || status == 304 || !response.getCookies().isEmpty()
                || headers.containsKey(HttpHeaders.SET_COOKIE) || headers.containsKey(HttpHeaders.VARY)
                || restrictedCacheControl(headers.getCacheControl())) {
            return Optional.empty();
        }
        Set<String> excluded = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
        excluded.addAll(HOP_BY_HOP);
        excluded.addAll(headers.getConnection());
        excluded.add(HttpHeaders.CONTENT_LENGTH);
        Map<String, List<String>> savedHeaders = new LinkedHashMap<>();
        headers.forEach((name, values) -> {
            if (!excluded.contains(name)) {
                savedHeaders.put(name, new ArrayList<>(values));
            }
        });
        return Optional.of(new CachedResponse(new Metadata(status, savedHeaders), body));
    }

    static boolean restrictedCacheControl(final String value) {
        if (StringUtils.isBlank(value)) {
            return false;
        }
        for (String directive : value.split(",")) {
            String name = StringUtils.substringBefore(directive, "=").trim();
            if ("private".equalsIgnoreCase(name) || "no-store".equalsIgnoreCase(name) || "no-cache".equalsIgnoreCase(name)) {
                return true;
            }
        }
        return false;
    }

    byte[] encode() {
        byte[] json = GSON.toJson(metadata).getBytes(StandardCharsets.UTF_8);
        return ByteBuffer.allocate(Math.addExact(Math.addExact(Integer.BYTES, json.length), body.length))
                .putInt(json.length).put(json).put(body).array();
    }

    static Optional<CachedResponse> decode(final byte[] packet) {
        try {
            if (packet.length < Integer.BYTES) {
                return Optional.empty();
            }
            ByteBuffer buffer = ByteBuffer.wrap(packet);
            int length = buffer.getInt();
            if (length <= 0 || length > buffer.remaining()) {
                return Optional.empty();
            }
            byte[] json = new byte[length];
            buffer.get(json);
            Metadata metadata = GSON.fromJson(new String(json, StandardCharsets.UTF_8), Metadata.class);
            if (Objects.isNull(metadata) || metadata.status < 200 || metadata.status > 999 || metadata.status == 206 || metadata.status == 304
                    || Objects.isNull(metadata.headers) || !validHeaders(metadata.headers)) {
                return Optional.empty();
            }
            HttpHeaders headers = new HttpHeaders();
            headers.putAll(metadata.headers);
            if (headers.containsKey(HttpHeaders.SET_COOKIE) || headers.containsKey(HttpHeaders.VARY) || restrictedCacheControl(headers.getCacheControl())
                    || HOP_BY_HOP.stream().anyMatch(headers::containsKey)) {
                return Optional.empty();
            }
            return Optional.of(new CachedResponse(metadata, Arrays.copyOfRange(packet, buffer.position(), packet.length)));
        } catch (RuntimeException ex) {
            return Optional.empty();
        }
    }

    Mono<Void> writeTo(final ServerHttpResponse response) {
        response.setStatusCode(HttpStatusCode.valueOf(metadata.status));
        metadata.headers.forEach((name, values) -> response.getHeaders().put(name, new ArrayList<>(values)));
        response.getHeaders().remove(HttpHeaders.TRANSFER_ENCODING);
        if (metadata.status == HttpStatus.NO_CONTENT.value()) {
            response.getHeaders().remove(HttpHeaders.CONTENT_LENGTH);
            return response.setComplete();
        }
        response.getHeaders().setContentLength(body.length);
        return body.length == 0 ? response.setComplete() : response.writeWith(Mono.just(response.bufferFactory().wrap(body)));
    }

    byte[] getBody() {
        return body;
    }

    private static boolean validHeaders(final Map<String, List<String>> headers) {
        return headers.entrySet().stream().allMatch(entry -> Objects.nonNull(entry.getKey()) && HEADER_NAME.matcher(entry.getKey()).matches()
                && Objects.nonNull(entry.getValue()) && entry.getValue().stream().allMatch(value -> Objects.nonNull(value)
                && value.chars().noneMatch(character -> character < 32 && character != '\t' || character == 127)));
    }

    private static final class Metadata {

        private final int status;

        private final Map<String, List<String>> headers;

        private Metadata(final int status, final Map<String, List<String>> headers) {
            this.status = status;
            this.headers = headers;
        }
    }
}
