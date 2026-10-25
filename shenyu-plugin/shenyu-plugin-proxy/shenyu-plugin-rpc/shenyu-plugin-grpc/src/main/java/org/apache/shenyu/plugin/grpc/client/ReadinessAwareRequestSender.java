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

package org.apache.shenyu.plugin.grpc.client;

import com.google.protobuf.DynamicMessage;
import io.grpc.stub.ClientCallStreamObserver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Sends stream requests only while the transport reports outbound readiness.
 * The request list is read as an immutable snapshot; no additional queue is introduced.
 */
final class ReadinessAwareRequestSender {

    private static final Logger LOG = LoggerFactory.getLogger(ReadinessAwareRequestSender.class);

    private final List<DynamicMessage> requests;

    private final ClientCallStreamObserver<DynamicMessage> observer;

    private final AtomicInteger cursor = new AtomicInteger();

    private final AtomicBoolean draining = new AtomicBoolean();

    private volatile boolean terminal;

    ReadinessAwareRequestSender(final List<DynamicMessage> requests, final ClientCallStreamObserver<DynamicMessage> observer) {
        this.requests = requests;
        this.observer = observer;
        observer.setOnReadyHandler(this::drain);
    }

    void start() {
        drain();
    }

    void terminate() {
        terminal = true;
    }

    private void drain() {
        while (true) {
            if (!draining.compareAndSet(false, true)) {
                return;
            }
            try {
                drainLoop();
            } finally {
                draining.set(false);
            }
            if (terminal || cursor.get() >= requests.size() || !observer.isReady()) {
                return;
            }
        }
    }

    private void drainLoop() {
        while (!terminal && cursor.get() < requests.size() && observer.isReady()) {
            int index = cursor.getAndIncrement();
            try {
                observer.onNext(requests.get(index));
            } catch (RuntimeException ex) {
                failSend(index, ex);
                return;
            }
        }
        if (!terminal && cursor.get() >= requests.size()) {
            terminal = true;
            try {
                observer.onCompleted();
            } catch (RuntimeException ex) {
                LOG.warn("Failed to complete the gRPC stream request", ex);
                cancelStream(ex);
            }
        }
    }

    private void failSend(final int index, final RuntimeException cause) {
        terminal = true;
        LOG.warn("Failed to send gRPC stream request at index {}", index, cause);
        cancelStream(cause);
    }

    private void cancelStream(final RuntimeException cause) {
        try {
            observer.cancel("failed to send stream request", cause);
        } catch (RuntimeException ex) {
            LOG.warn("Failed to cancel the gRPC stream request", ex);
        }
    }
}
