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

package org.apache.shenyu.plugin.ai.proxy.enhanced;

import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.springframework.core.io.buffer.DataBufferLimitException;
import org.springframework.http.HttpStatus;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link AiProxyFailureMapper}.
 */
class AiProxyFailureMapperTest {

    private final AiProxyFailureMapper mapper = new AiProxyFailureMapper();

    @ParameterizedTest
    @EnumSource(AiTransportException.Kind.class)
    void mapsEveryTransportFailureKind(final AiTransportException.Kind kind) {
        AiProxyFailureResponse response = mapper.map(new AiTransportException(kind, "transport failure", null));
        HttpStatus expected = kind == AiTransportException.Kind.REQUEST_TOO_LARGE
                ? HttpStatus.PAYLOAD_TOO_LARGE : HttpStatus.BAD_GATEWAY;
        assertEquals(expected, response.getStatus());
        assertTrue(response.getHeaders().isEmpty());
    }

    @ParameterizedTest
    @EnumSource(AiProxyException.Kind.class)
    void mapsEveryProxyFailureKind(final AiProxyException.Kind kind) {
        AiProxyFailureResponse response = mapper.map(new AiProxyException(kind, "proxy failure", null));
        HttpStatus expected = kind == AiProxyException.Kind.REQUEST
                ? HttpStatus.BAD_REQUEST : HttpStatus.BAD_GATEWAY;
        assertEquals(expected, response.getStatus());
    }

    @Test
    void mapsFrameworkAndUnknownFailures() {
        assertEquals(HttpStatus.PAYLOAD_TOO_LARGE,
                mapper.map(new DataBufferLimitException("too large")).getStatus());
        assertEquals(HttpStatus.BAD_REQUEST,
                mapper.map(new ShenyuException("bad request")).getStatus());
        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR,
                mapper.map(new IllegalStateException("unexpected")).getStatus());
    }
}
