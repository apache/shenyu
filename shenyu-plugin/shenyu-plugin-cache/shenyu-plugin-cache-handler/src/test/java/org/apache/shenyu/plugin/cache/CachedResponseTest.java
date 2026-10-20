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
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import reactor.test.StepVerifier;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
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
        CachedResponse captured = CachedResponse.capture(source, body).orElseThrow();
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
