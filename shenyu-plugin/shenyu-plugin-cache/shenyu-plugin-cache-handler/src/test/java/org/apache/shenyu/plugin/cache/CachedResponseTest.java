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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import reactor.test.StepVerifier;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests complete cache entry encoding and rejection of invalid cached metadata.
 */
class CachedResponseTest {

    @Test
    void testBinaryBodyAndHeadersRoundTrip() {
        MockServerHttpResponse source = new MockServerHttpResponse();
        source.setStatusCode(HttpStatus.FOUND);
        source.getHeaders().set(HttpHeaders.LOCATION, "/destination");
        source.getHeaders().addAll("X-Values", List.of("first", "second"));
        byte[] body = new byte[] {0, -1, 12, 42, -127};
        CachedResponse captured = CachedResponse.capture(request(), source, body).orElseThrow();
        source.getHeaders().set("X-Values", "changed");
        CachedResponse decoded = CachedResponse.decode(captured.encode()).orElseThrow();
        assertArrayEquals(body, decoded.getBody());
        MockServerHttpResponse target = new MockServerHttpResponse();

        StepVerifier.create(decoded.writeTo(target)).verifyComplete();

        assertEquals(HttpStatus.FOUND, target.getStatusCode());
        assertEquals("/destination", target.getHeaders().getFirst(HttpHeaders.LOCATION));
        assertEquals(List.of("first", "second"), target.getHeaders().get("X-Values"));
        assertEquals(body.length, target.getHeaders().getContentLength());
    }

    @Test
    void testVarySelectsResponseByRequestHeaderValue() {
        MockServerHttpResponse source = new MockServerHttpResponse();
        source.getHeaders().setVary(List.of("Origin", "Accept-Language"));
        CachedResponse captured = CachedResponse.capture(
                request(HttpHeaders.ORIGIN, "https://a.example", HttpHeaders.ACCEPT_LANGUAGE, "zh"), source, new byte[] {1}).orElseThrow();
        CachedResponse decoded = CachedResponse.decode(captured.encode()).orElseThrow();

        assertTrue(decoded.matches(request(HttpHeaders.ORIGIN, "https://a.example", HttpHeaders.ACCEPT_LANGUAGE, "zh")));
        assertFalse(decoded.matches(request(HttpHeaders.ORIGIN, "https://b.example", HttpHeaders.ACCEPT_LANGUAGE, "zh")));
        assertFalse(decoded.matches(request(HttpHeaders.ORIGIN, "https://a.example", HttpHeaders.ACCEPT_LANGUAGE, "en")));
        assertFalse(decoded.matches(request()));
    }

    @Test
    void testResponseWithoutVaryIsReusable() {
        CachedResponse captured = CachedResponse.capture(request(), new MockServerHttpResponse(), new byte[] {1}).orElseThrow();

        assertTrue(CachedResponse.decode(captured.encode()).orElseThrow().matches(request(HttpHeaders.ORIGIN, "https://a.example")));
    }

    @Test
    void testVaryAllIsNotStored() {
        MockServerHttpResponse source = new MockServerHttpResponse();
        source.getHeaders().setVary(List.of("*"));

        assertTrue(CachedResponse.capture(request(), source, new byte[0]).isEmpty());
    }

    @Test
    void testSelectingValuesMustMatchVary() {
        MockServerHttpResponse source = new MockServerHttpResponse();
        source.getHeaders().setVary(List.of("Origin"));

        assertTrue(CachedResponse.decode(metadata("{\"status\":200,\"headers\":{\"vary\":[\"Origin\"]},\"selectingValues\":{}}")).isEmpty());
        assertTrue(CachedResponse.decode(metadata("{\"status\":200,\"headers\":{},\"selectingValues\":{\"Origin\":[]}}")).isEmpty());
        assertTrue(CachedResponse.decode(metadata("{\"status\":200,\"headers\":{\"vary\":[\"*\"]},\"selectingValues\":{}}")).isEmpty());
        assertTrue(CachedResponse.decode(metadata("{\"status\":200,\"headers\":{\"vary\":[\"Origin\"]},"
                + "\"selectingValues\":{\"Origin\":[\"bad\\r\\nvalue\"]}}")).isEmpty());
        assertTrue(CachedResponse.decode(metadata("{\"status\":200,\"headers\":{}}")).orElseThrow().matches(request()));
        assertTrue(CachedResponse.decode(CachedResponse.capture(request(HttpHeaders.ORIGIN, "https://a.example"), source, new byte[0])
                .orElseThrow().encode()).isPresent());
    }

    private static byte[] metadata(final String metadata) {
        byte[] json = metadata.getBytes(StandardCharsets.UTF_8);
        return ByteBuffer.allocate(Integer.BYTES + json.length).putInt(json.length).put(json).array();
    }

    private static MockServerHttpRequest request(final String... headers) {
        MockServerHttpRequest.BaseBuilder<?> builder = MockServerHttpRequest.get("http://gateway.example/cache");
        for (int index = 0; index < headers.length; index += 2) {
            builder.header(headers[index], headers[index + 1]);
        }
        return builder.build();
    }

    @ParameterizedTest
    @ValueSource(ints = {-1, 0, 1000, Integer.MAX_VALUE})
    void testInvalidMetadataLengthIsIgnored(final int length) {
        assertTrue(CachedResponse.decode(ByteBuffer.allocate(5).putInt(length).put((byte) 0).array()).isEmpty());
    }

    @ParameterizedTest
    @ValueSource(strings = {"not-json", "null", "{\"status\":99,\"headers\":{}}", "{\"status\":200,\"headers\":null}",
        "{\"status\":200,\"headers\":{\"X-Test\":[null]}}", "{\"status\":200,\"headers\":{\"Bad Header\":[\"value\"]}}",
        "{\"status\":200,\"headers\":{\"X-Test\":[\"bad\\r\\nvalue\"]}}", "{\"status\":200,\"headers\":{\"set-cookie\":[\"session=user\"]}}",
        "{\"status\":200,\"headers\":{\"cache-control\":[\"private\"]}}", "{\"status\":200,\"headers\":{\"connection\":[\"close\"]}}"})
    void testInvalidMetadataIsIgnored(final String metadata) {
        byte[] json = metadata.getBytes(StandardCharsets.UTF_8);
        byte[] packet = ByteBuffer.allocate(Integer.BYTES + json.length).putInt(json.length).put(json).array();

        assertTrue(CachedResponse.decode(packet).isEmpty());
    }

    @Test
    void testTruncatedPrefixIsIgnored() {
        assertTrue(CachedResponse.decode(new byte[] {0, 1}).isEmpty());
    }
}
