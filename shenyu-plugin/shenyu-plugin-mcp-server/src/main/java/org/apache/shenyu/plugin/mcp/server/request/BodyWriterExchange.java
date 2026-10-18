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

package org.apache.shenyu.plugin.mcp.server.request;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.plugin.base.support.BodyInserterContext;
import org.apache.shenyu.plugin.base.support.CachedBodyOutputMessage;
import org.apache.shenyu.plugin.base.utils.ResponseUtils;
import org.springframework.core.ResolvableType;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ReactiveHttpOutputMessage;
import org.springframework.http.client.MultipartBodyBuilder;
import org.springframework.http.codec.FormHttpMessageWriter;
import org.springframework.http.codec.multipart.MultipartHttpMessageWriter;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MimeTypeUtils;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.BodyInserter;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.ServerWebExchangeDecorator;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Base64;
import java.util.Collections;
import java.util.Objects;
import java.util.Set;

/**
 * Body Writer Exchange for MCP tool requests.
 * 
 * <p>Decorates ServerWebExchange to properly handle request body writing for different content types.
 * Supports JSON, form-urlencoded and multipart bodies.</p>
 *
 * @since 2.7.0.2
 */
public class BodyWriterExchange extends ServerWebExchangeDecorator {

    private static final Set<String> FILE_FIELDS = Set.of("filename", "content", "contentType");

    private final CachedBodyOutputMessage cachedBodyOutputMessage;

    private final Mono<Void> bodyInsertion;

    public BodyWriterExchange(final ServerWebExchange delegate, final String body) {
        this(delegate, body, null);
    }

    public BodyWriterExchange(final ServerWebExchange delegate, final JsonObject body) {
        this(delegate, null, Objects.requireNonNull(body, "body"));
    }

    private BodyWriterExchange(final ServerWebExchange delegate, final String body, final JsonObject bodyJson) {
        super(delegate);
        this.cachedBodyOutputMessage = ResponseUtils.newCachedBodyOutputMessage(delegate);
        this.cachedBodyOutputMessage.getHeaders().remove(HttpHeaders.TRANSFER_ENCODING);
        final MediaType contentType = delegate.getRequest().getHeaders().getContentType();
        if (Objects.nonNull(contentType) && !contentType.isConcrete()) {
            throw new IllegalArgumentException("MCP request body Content-Type must be concrete");
        }
        if (Objects.nonNull(contentType) && MediaType.APPLICATION_FORM_URLENCODED.isCompatibleWith(contentType)) {
            this.bodyInsertion = new FormHttpMessageWriter().write(Mono.just(buildFormData(resolveBody(body, bodyJson))),
                    ResolvableType.forClassWithGenerics(MultiValueMap.class, String.class, String.class),
                    contentType, cachedBodyOutputMessage, Collections.emptyMap());
        } else if (Objects.nonNull(contentType) && MediaType.MULTIPART_FORM_DATA.isCompatibleWith(contentType)) {
            final MultiValueMap<String, ?> parts = buildMultipartData(resolveBody(body, bodyJson));
            final RequestMultipartWriter writer = new RequestMultipartWriter(contentType);
            cachedBodyOutputMessage.getHeaders().setContentType(writer.contentType);
            this.bodyInsertion = writer.write(Mono.just(parts), ResolvableType.forClass(MultiValueMap.class),
                    writer.contentType, cachedBodyOutputMessage, Collections.emptyMap());
        } else if (Objects.isNull(contentType) || MediaType.APPLICATION_JSON.isCompatibleWith(contentType) || contentType.getSubtype().endsWith("+json")) {
            final String json = Objects.nonNull(bodyJson) ? (bodyJson.isEmpty() ? "" : bodyJson.toString()) : body;
            final BodyInserter<String, ReactiveHttpOutputMessage> bodyInserter = BodyInserters.fromValue(json);
            this.bodyInsertion = bodyInserter.insert(cachedBodyOutputMessage, new BodyInserterContext());
        } else {
            throw new IllegalArgumentException("Unsupported MCP request body Content-Type");
        }
    }

    private JsonObject resolveBody(final String body, final JsonObject bodyJson) {
        return Objects.nonNull(bodyJson) ? bodyJson : JsonParser.parseString(body).getAsJsonObject();
    }

    private MultiValueMap<String, String> buildFormData(final JsonObject fields) {
        final MultiValueMap<String, String> formData = new LinkedMultiValueMap<>();
        fields.entrySet().forEach(entry -> {
            final JsonElement value = entry.getValue();
            if (value.isJsonArray()) {
                value.getAsJsonArray().forEach(element -> addFormValue(formData, entry.getKey(), element));
            } else {
                addFormValue(formData, entry.getKey(), value);
            }
        });
        return formData;
    }

    private void addFormValue(final MultiValueMap<String, String> formData, final String name, final JsonElement value) {
        if (!value.isJsonNull() && !value.isJsonPrimitive()) {
            throw new IllegalArgumentException("Form fields must contain scalar values or arrays of scalar values");
        }
        formData.add(name, value.isJsonNull() ? "" : value.getAsString());
    }

    private MultiValueMap<String, ?> buildMultipartData(final JsonObject fields) {
        final MultipartBodyBuilder builder = new MultipartBodyBuilder();
        fields.entrySet().forEach(entry -> {
            validatePartName(entry.getKey());
            final JsonElement value = entry.getValue();
            if (value.isJsonArray()) {
                value.getAsJsonArray().forEach(element -> addMultipartValue(builder, entry.getKey(), element));
            } else {
                addMultipartValue(builder, entry.getKey(), value);
            }
        });
        return builder.build();
    }

    private void addMultipartValue(final MultipartBodyBuilder builder, final String name, final JsonElement value) {
        if (value.isJsonObject()) {
            final JsonObject file = value.getAsJsonObject();
            if (!FILE_FIELDS.containsAll(file.keySet())) {
                throw new IllegalArgumentException("Multipart file objects only support filename, content and contentType");
            }
            final String filename = requiredFileString(file, "filename");
            validatePartName(filename);
            final String mediaType = file.has("contentType") ? requiredFileString(file, "contentType") : MediaType.APPLICATION_OCTET_STREAM_VALUE;
            if (mediaType.contains("\r") || mediaType.contains("\n")) {
                throw new IllegalArgumentException("Multipart file contentType must contain no line breaks");
            }
            final MediaType contentType = MediaType.parseMediaType(mediaType);
            if (!contentType.isConcrete()) {
                throw new IllegalArgumentException("Multipart file contentType must be concrete");
            }
            final byte[] bytes = Base64.getDecoder().decode(requiredFileString(file, "content"));
            builder.part(name, new ByteArrayResource(bytes)).filename(filename).contentType(contentType);
        } else if (value.isJsonNull() || value.isJsonPrimitive()) {
            builder.part(name, value.isJsonNull() ? "" : value.getAsString());
        } else {
            throw new IllegalArgumentException("Multipart fields must contain scalar values, file objects or flat arrays");
        }
    }

    private String requiredFileString(final JsonObject file, final String key) {
        final JsonElement value = file.get(key);
        if (Objects.isNull(value) || !value.isJsonPrimitive() || !value.getAsJsonPrimitive().isString()) {
            throw new IllegalArgumentException("Multipart file " + key + " must be a string");
        }
        return value.getAsString();
    }

    private void validatePartName(final String name) {
        if (name.isBlank() || name.contains("\r") || name.contains("\n")) {
            throw new IllegalArgumentException("Multipart part names and filenames must be nonblank and contain no line breaks");
        }
    }

    /**
     * Returns a decorated ServerHttpRequest with the correct body and headers for downstream processing.
     *
     * @return the decorated ServerHttpRequest
     */
    @Override
    public ServerHttpRequest getRequest() {
        return new ServerHttpRequestDecorator(super.getRequest()) {
            @Override
            public Flux<DataBuffer> getBody() {
                return bodyInsertion.thenMany(Flux.defer(cachedBodyOutputMessage::getBody));
            }

            @Override
            public HttpHeaders getHeaders() {
                final HttpHeaders newHeaders = new HttpHeaders();
                newHeaders.putAll(cachedBodyOutputMessage.getHeaders());
                newHeaders.remove(HttpHeaders.CONTENT_LENGTH);
                newHeaders.remove(HttpHeaders.TRANSFER_ENCODING);
                return newHeaders;
            }
        };
    }

    private static final class RequestMultipartWriter extends MultipartHttpMessageWriter {

        private final byte[] boundary = MimeTypeUtils.generateMultipartBoundary();

        private final MediaType contentType;

        private RequestMultipartWriter(final MediaType mediaType) {
            if (Objects.nonNull(mediaType.getCharset())) {
                setCharset(mediaType.getCharset());
            }
            // Downstream clients can read headers before subscribing to the body.
            this.contentType = getMultipartMediaType(mediaType, boundary);
        }

        @Override
        protected byte[] generateMultipartBoundary() {
            return boundary;
        }
    }
}
