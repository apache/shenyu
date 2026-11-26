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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service.failure;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Signal;
import reactor.core.publisher.SynchronousSink;

import java.io.ByteArrayOutputStream;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Inspects a bounded prefix while retaining one subscription through response consumption.
 */
final class AiProxyCredentialFailureInspector {

    public static final int MAX_INSPECTION_BYTES = 16 * 1024;

    private AiProxyCredentialFailureInspector() {
    }

    public static <T> Mono<T> inspect(final AiUpstreamResponse response, final Predicate<byte[]> classifier,
                                      final Function<Inspection, Mono<T>> consumer) {
        return Mono.defer(() -> {
            final Accumulator accumulator = new Accumulator();
            return response.getBody().materialize().<Frame>handle(accumulator::accept)
                    .switchOnFirst((signal, frames) -> {
                        if (signal.isOnError()) {
                            return Flux.error(Objects.requireNonNull(signal.getThrowable()));
                        }
                        final Frame first = Objects.requireNonNull(signal.get());
                        final boolean classified = first.signal().isOnComplete() && classifier.test(first.prefix());
                        final Flux<byte[]> body = frames.concatMap(Frame::replay, 1);
                        final AiUpstreamResponse preserved = new AiUpstreamResponse(response.getStatusCode(), response.getHeaders(), body);
                        return Mono.defer(() -> consumer.apply(new Inspection(preserved, classified))).flux();
                    }).next().doFinally(ignored -> accumulator.clear());
        });
    }

    public record Inspection(AiUpstreamResponse response, boolean credentialFailure) {
    }

    private record Frame(byte[] prefix, Signal<byte[]> signal) {

        private Flux<byte[]> replay() {
            final Flux<byte[]> buffered = Objects.isNull(prefix) ? Flux.empty() : Flux.just(prefix);
            return buffered.concatWith(Flux.just(signal).dematerialize());
        }
    }

    private static final class Accumulator {

        private volatile ByteArrayOutputStream buffer = new ByteArrayOutputStream();

        private boolean received;

        private boolean forwarding;

        private void accept(final Signal<byte[]> signal, final SynchronousSink<Frame> sink) {
            if (forwarding) {
                sink.next(new Frame(null, signal));
                return;
            }
            final ByteArrayOutputStream current = buffer;
            if (Objects.isNull(current)) {
                return;
            }
            if (signal.isOnNext()) {
                final byte[] bytes = Objects.requireNonNull(signal.get());
                if (bytes.length <= MAX_INSPECTION_BYTES - current.size()) {
                    current.writeBytes(bytes);
                    received = true;
                    return;
                }
            }
            // Completion permits classification; overflow/error only releases the original bytes.
            forwarding = true;
            clear();
            sink.next(new Frame(received ? current.toByteArray() : null, signal));
        }

        private void clear() {
            buffer = null;
        }
    }
}
