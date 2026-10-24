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

import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.ResolvableType;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.codec.FormHttpMessageReader;
import org.springframework.http.codec.multipart.DefaultPartHttpMessageReader;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.http.codec.multipart.FormFieldPart;
import org.springframework.http.codec.multipart.Part;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.util.MultiValueMap;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for MCP request body encoding.
 */
class BodyWriterExchangeTest {

    @Test
    void testFormBodyRoundTripsScalarsAndRepeatedValues() {
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/submit")
                .contentType(MediaType.APPLICATION_FORM_URLENCODED).build());
        final ServerHttpRequest request = new BodyWriterExchange(exchange,
                "{\"na&me\":\"Ada & Bob+中文=1\",\"count\":2,\"enabled\":true,\"empty\":\"\",\"missing\":null,\"tag\":[\"x\",\"y\"]}").getRequest();

        final MultiValueMap<String, String> fields = readForm(request);

        assertNotNull(fields);
        assertEquals("Ada & Bob+中文=1", fields.getFirst("na&me"));
        assertEquals("2", fields.getFirst("count"));
        assertEquals("true", fields.getFirst("enabled"));
        assertEquals("", fields.getFirst("empty"));
        assertEquals("", fields.getFirst("missing"));
        assertEquals(List.of("x", "y"), fields.get("tag"));
    }

    @Test
    void testFormBodyUsesConfiguredCharset() {
        final MediaType contentType = MediaType.parseMediaType("application/x-www-form-urlencoded;charset=ISO-8859-1");
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/submit")
                .contentType(contentType).build());
        final ServerHttpRequest request = new BodyWriterExchange(exchange, "{\"name\":\"André\"}").getRequest();

        final MultiValueMap<String, String> fields = readForm(request);

        assertNotNull(fields);
        assertEquals(contentType, request.getHeaders().getContentType());
        assertEquals("André", fields.getFirst("name"));
    }

    @Test
    void testRewrittenBodyDoesNotReuseIncomingFramingHeaders() {
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/submit")
                .contentType(MediaType.APPLICATION_FORM_URLENCODED)
                .contentLength(999)
                .header(HttpHeaders.TRANSFER_ENCODING, "chunked").build());
        final ServerHttpRequest request = new BodyWriterExchange(exchange, "{\"name\":\"Ada\"}").getRequest();

        assertNotEquals(999, request.getHeaders().getContentLength());
        assertFalse(request.getHeaders().containsKey(HttpHeaders.TRANSFER_ENCODING));
        final MultiValueMap<String, String> fields = readForm(request);
        assertNotNull(fields);
        assertEquals("Ada", fields.getFirst("name"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"field\":{}}", "{\"field\":[{}]}", "{\"field\":[[]]}"})
    void testFormBodyRejectsNestedValues(final String body) {
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/submit")
                .contentType(MediaType.APPLICATION_FORM_URLENCODED).build());

        final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> new BodyWriterExchange(exchange, body));

        assertEquals("Form fields must contain scalar values or arrays of scalar values", exception.getMessage());
    }

    private MultiValueMap<String, String> readForm(final ServerHttpRequest request) {
        return new FormHttpMessageReader().readMono(
                ResolvableType.forClassWithGenerics(MultiValueMap.class, String.class, String.class),
                request, Collections.emptyMap()).block();
    }

    @Test
    void testMultipartBodyCanBeParsedBeforeBodySubscription() {
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/upload")
                .contentType(MediaType.parseMediaType("multipart/form-data;boundary=old-boundary")).build());
        final ServerHttpRequest request = new BodyWriterExchange(exchange,
                "{\"file\":{\"filename\":\"hello.txt\",\"content\":\"aGVsbG8=\",\"contentType\":\"text/plain\"},\"note\":\"中文 & text\"}").getRequest();

        final MediaType contentType = request.getHeaders().getContentType();
        assertNotNull(contentType);
        assertNotNull(contentType.getParameter("boundary"));
        assertNotEquals("old-boundary", contentType.getParameter("boundary"));
        final List<Part> parts = readParts(request);
        try {
            assertEquals(2, parts.size());
            final FilePart file = assertInstanceOf(FilePart.class, parts.get(0));
            assertEquals("file", file.name());
            assertEquals("hello.txt", file.filename());
            assertEquals(MediaType.TEXT_PLAIN, file.headers().getContentType());
            assertArrayEquals("hello".getBytes(StandardCharsets.UTF_8), readPartBytes(file));
            assertEquals("中文 & text", assertInstanceOf(FormFieldPart.class, parts.get(1)).value());
        } finally {
            parts.forEach(part -> part.delete().block());
        }
    }

    @Test
    void testMultipartBodySupportsRepeatedFilesAndBinaryData() {
        final byte[] bytes = new byte[]{0, 1, (byte) 255, 13, 10};
        final String content = Base64.getEncoder().encodeToString(bytes);
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/upload")
                .contentType(MediaType.MULTIPART_FORM_DATA).build());
        final ServerHttpRequest request = new BodyWriterExchange(exchange,
                "{\"files\":[{\"filename\":\"二进制.bin\",\"content\":\"" + content
                        + "\"},{\"filename\":\"empty.bin\",\"content\":\"\"}],\"tags\":[\"one\",\"two\"]}").getRequest();

        final List<Part> parts = readParts(request);
        try {
            assertEquals(4, parts.size());
            final FilePart first = assertInstanceOf(FilePart.class, parts.get(0));
            assertEquals("files", first.name());
            assertEquals("二进制.bin", first.filename());
            assertEquals(MediaType.APPLICATION_OCTET_STREAM, first.headers().getContentType());
            assertArrayEquals(bytes, readPartBytes(first));
            final FilePart second = assertInstanceOf(FilePart.class, parts.get(1));
            assertEquals("files", second.name());
            assertEquals("empty.bin", second.filename());
            assertArrayEquals(new byte[0], readPartBytes(second));
            assertEquals("one", assertInstanceOf(FormFieldPart.class, parts.get(2)).value());
            assertEquals("two", assertInstanceOf(FormFieldPart.class, parts.get(3)).value());
        } finally {
            parts.forEach(part -> part.delete().block());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"file\":{\"filename\":\"x.txt\",\"content\":\"!!!\"}}", "{\"file\":{\"content\":\"\"}}",
        "{\"file\":{\"filename\":\"x.txt\"}}", "{\"file\":{\"filename\":\"\",\"content\":\"\"}}",
        "{\"file\":{\"filename\":\"x.txt\",\"content\":1}}", "{\"file\":[[]]}",
        "{\"file\":{\"filename\":\"x.txt\",\"content\":\"\",\"contentType\":\"*/*\"}}",
        "{\"file\":{\"filename\":\"x.txt\",\"content\":\"\",\"unknown\":true}}",
        "{\"file\":{\"filename\":\"x\\r\\n.txt\",\"content\":\"\"}}",
        "{\"file\":{\"filename\":\"x.txt\",\"content\":\"\",\"contentType\":\"text/plain;x=\\\"a\\r\\nb\\\"\"}}"})
    void testMultipartBodyRejectsInvalidFiles(final String body) {
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/upload")
                .contentType(MediaType.MULTIPART_FORM_DATA).build());

        assertThrows(IllegalArgumentException.class, () -> new BodyWriterExchange(exchange, body));
    }

    @ParameterizedTest
    @ValueSource(strings = {"text/plain", "application/xml", "application/graphql", "application/octet-stream",
        "application/vnd.example.custom", "text/plain;charset=ISO-8859-1"})
    void testOtherConcreteContentTypesPreserveRawBody(final String contentType) {
        final MediaType mediaType = MediaType.parseMediaType(contentType);
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/submit")
                .contentType(mediaType).build());
        final String body = "André & <value>=\"unchanged\"\r\n";
        final ServerHttpRequest request = new BodyWriterExchange(exchange, body).getRequest();

        assertEquals(mediaType, request.getHeaders().getContentType());
        assertArrayEquals(body.getBytes(Objects.isNull(mediaType.getCharset()) ? StandardCharsets.UTF_8 : mediaType.getCharset()), readBodyBytes(request));
    }

    @ParameterizedTest
    @ValueSource(strings = {"application/json", "application/vnd.example+json"})
    void testEmptyJsonObjectRemainsAnObject(final String contentType) {
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/submit")
                .header(HttpHeaders.CONTENT_TYPE, contentType).build());
        final ServerHttpRequest request = new BodyWriterExchange(exchange, new JsonObject()).getRequest();

        assertArrayEquals("{}".getBytes(StandardCharsets.UTF_8), readBodyBytes(request));
    }

    @ParameterizedTest
    @ValueSource(strings = {"application/*", "multipart/*", "*/*"})
    void testWildcardContentTypeIsRejected(final String contentType) {
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/submit")
                .header(HttpHeaders.CONTENT_TYPE, contentType).build());

        assertThrows(IllegalArgumentException.class, () -> new BodyWriterExchange(exchange, "{\"name\":\"Ada\"}"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", " ", "a\rb", "a\nb", "a\"; filename=payload.sh", "a\\b"})
    void testMultipartRejectsUnsafeFieldNamesAndFilenames(final String name) {
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/upload")
                .contentType(MediaType.MULTIPART_FORM_DATA).build());
        final JsonObject fields = new JsonObject();
        fields.addProperty(name, "text");
        assertThrows(IllegalArgumentException.class, () -> new BodyWriterExchange(exchange, fields));

        final JsonObject file = new JsonObject();
        file.addProperty("filename", name);
        file.addProperty("content", "");
        final JsonObject files = new JsonObject();
        files.add("file", file);
        assertThrows(IllegalArgumentException.class, () -> new BodyWriterExchange(exchange, files));
    }

    @Test
    void testMultipartPreservesUnicodeAndSpacesInNames() {
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/upload")
                .contentType(MediaType.MULTIPART_FORM_DATA).build());
        final ServerHttpRequest request = new BodyWriterExchange(exchange,
                "{\"文件 列表\":{\"filename\":\"报告 2026.txt\",\"content\":\"aGVsbG8=\"}}").getRequest();
        final List<Part> parts = readParts(request);
        try {
            assertEquals(1, parts.size());
            final FilePart file = assertInstanceOf(FilePart.class, parts.get(0));
            assertEquals("文件 列表", file.name());
            assertEquals("报告 2026.txt", file.filename());
            assertArrayEquals("hello".getBytes(StandardCharsets.UTF_8), readPartBytes(file));
        } finally {
            parts.forEach(part -> part.delete().block());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"{", "{\"field\":"})
    void testMalformedFormAndMultipartJsonHasDescriptiveError(final String body) {
        for (MediaType contentType : List.of(MediaType.APPLICATION_FORM_URLENCODED, MediaType.MULTIPART_FORM_DATA)) {
            final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/submit")
                    .contentType(contentType).build());
            final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> new BodyWriterExchange(exchange, body));
            assertEquals("MCP form or multipart body must be valid JSON", exception.getMessage());
            assertInstanceOf(JsonParseException.class, exception.getCause());
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"[]", "null", "true", "42", "\"text\""})
    void testFormAndMultipartRequireJsonObjects(final String body) {
        for (MediaType contentType : List.of(MediaType.APPLICATION_FORM_URLENCODED, MediaType.MULTIPART_FORM_DATA)) {
            final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/submit")
                    .contentType(contentType).build());
            final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> new BodyWriterExchange(exchange, body));
            assertEquals("MCP form or multipart body must be a JSON object", exception.getMessage());
        }
    }

    private byte[] readBodyBytes(final ServerHttpRequest request) {
        return DataBufferUtils.join(request.getBody()).map(buffer -> {
            try {
                final byte[] bytes = new byte[buffer.readableByteCount()];
                buffer.read(bytes);
                return bytes;
            } finally {
                DataBufferUtils.release(buffer);
            }
        }).defaultIfEmpty(new byte[0]).block();
    }

    private List<Part> readParts(final ServerHttpRequest request) {
        final List<Part> parts = new DefaultPartHttpMessageReader().read(
                ResolvableType.forClass(Part.class), request, Collections.emptyMap()).collectList().block();
        assertNotNull(parts);
        assertTrue(parts.size() > 0);
        return parts;
    }

    private byte[] readPartBytes(final Part part) {
        return DataBufferUtils.join(part.content()).map(buffer -> {
            try {
                final byte[] bytes = new byte[buffer.readableByteCount()];
                buffer.read(bytes);
                return bytes;
            } finally {
                DataBufferUtils.release(buffer);
            }
        }).defaultIfEmpty(new byte[0]).block();
    }
}
