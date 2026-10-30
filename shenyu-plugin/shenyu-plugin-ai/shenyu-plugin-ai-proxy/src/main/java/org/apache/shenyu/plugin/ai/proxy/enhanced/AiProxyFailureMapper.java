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
import org.springframework.core.io.buffer.DataBufferLimitException;
import org.springframework.http.HttpStatus;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;

/**
 * Maps classified AI proxy failures to normalized client responses.
 */
final class AiProxyFailureMapper {

    private static final AiProxyFailureResponse INTERNAL_SERVER_ERROR = response(HttpStatus.INTERNAL_SERVER_ERROR);

    private final List<FailureMapping<?>> mappings = List.of(
            FailureMapping.of(DataBufferLimitException.class, ignored -> response(HttpStatus.PAYLOAD_TOO_LARGE)),
            FailureMapping.of(ShenyuException.class, ignored -> response(HttpStatus.BAD_REQUEST)),
            FailureMapping.of(AiTransportException.class, AiProxyFailureMapper::mapTransportFailure),
            FailureMapping.of(AiProxyException.class, AiProxyFailureMapper::mapProxyFailure));

    AiProxyFailureResponse map(final Throwable failure) {
        Objects.requireNonNull(failure, "failure must not be null");
        for (FailureMapping<?> mapping : mappings) {
            AiProxyFailureResponse response = mapping.map(failure);
            if (Objects.nonNull(response)) {
                return response;
            }
        }
        return INTERNAL_SERVER_ERROR;
    }

    private static AiProxyFailureResponse mapTransportFailure(final AiTransportException failure) {
        HttpStatus status = switch (failure.getKind()) {
            case REQUEST_TOO_LARGE -> HttpStatus.PAYLOAD_TOO_LARGE;
            case CONNECT_TIMEOUT,
                 CONNECTION_POOL_TIMEOUT,
                 RESPONSE_HEADER_TIMEOUT,
                 STREAM_IDLE_TIMEOUT,
                 STREAM_DURATION_TIMEOUT,
                 RESPONSE_TOO_LARGE,
                 DNS_FAILURE,
                 TLS_FAILURE,
                 CONNECTION_RESET,
                 IO_FAILURE,
                 UNKNOWN -> HttpStatus.BAD_GATEWAY;
        };
        return response(status);
    }

    private static AiProxyFailureResponse mapProxyFailure(final AiProxyException failure) {
        HttpStatus status = switch (failure.getKind()) {
            case REQUEST -> HttpStatus.BAD_REQUEST;
            case PROVIDER, TRANSPORT, PROTOCOL -> HttpStatus.BAD_GATEWAY;
        };
        return response(status);
    }

    private static AiProxyFailureResponse response(final HttpStatus status) {
        return new AiProxyFailureResponse(status);
    }

    private static final class FailureMapping<T extends Throwable> {

        private final Class<T> failureType;

        private final Function<T, AiProxyFailureResponse> mapper;

        private FailureMapping(final Class<T> failureType, final Function<T, AiProxyFailureResponse> mapper) {
            this.failureType = failureType;
            this.mapper = mapper;
        }

        private static <T extends Throwable> FailureMapping<T> of(
                final Class<T> failureType,
                final Function<T, AiProxyFailureResponse> mapper) {
            return new FailureMapping<>(failureType, mapper);
        }

        private AiProxyFailureResponse map(final Throwable failure) {
            if (!failureType.isInstance(failure)) {
                return null;
            }
            return mapper.apply(failureType.cast(failure));
        }
    }
}
