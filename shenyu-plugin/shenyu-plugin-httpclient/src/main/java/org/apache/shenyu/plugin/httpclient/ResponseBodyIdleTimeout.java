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

package org.apache.shenyu.plugin.httpclient;

import org.apache.shenyu.common.exception.StreamIdleTimeoutException;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;

import java.util.concurrent.atomic.AtomicBoolean;

/** Carries one request's idle interval and terminal transport signal in Reactor Context. */
public final class ResponseBodyIdleTimeout {

    private final long millis;

    private final Sinks.Empty<Void> failure;

    private final AtomicBoolean active = new AtomicBoolean(true);

    /**
     * Capture the matched rule's response-body idle interval.
     * @param millis interval in milliseconds
     */
    public ResponseBodyIdleTimeout(final long millis) {
        this(millis, Sinks.empty());
    }

    private ResponseBodyIdleTimeout(final long millis, final Sinks.Empty<Void> failure) {
        this.millis = millis;
        this.failure = failure;
    }

    /**
     * Return the configured interval.
     * @return milliseconds between body reads
     */
    public long millis() {
        return millis;
    }

    /**
     * Interrupt the exchange even when the downstream writer has yet to subscribe to the body.
     * @return the same failure to propagate on the upstream channel
     */
    public StreamIdleTimeoutException expire() {
        StreamIdleTimeoutException timeout = new StreamIdleTimeoutException(millis);
        if (active.compareAndSet(true, false)) {
            failure.tryEmitError(timeout);
        }
        return timeout;
    }

    <T> Mono<T> bindAttempt(final Mono<T> send) {
        // A timed-out header attempt may still have a body timer queued on its Netty event loop.
        ResponseBodyIdleTimeout attempt = new ResponseBodyIdleTimeout(millis, failure);
        return send.contextWrite(context -> context.put(ResponseBodyIdleTimeout.class, attempt))
                .doOnCancel(() -> attempt.active.set(false))
                .doOnError(error -> attempt.active.set(false));
    }

    Mono<Void> watch(final Mono<Void> exchange) {
        return Mono.firstWithSignal(failure.asMono(), exchange);
    }
}
