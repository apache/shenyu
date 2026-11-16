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
import org.apache.shenyu.protocol.grpc.message.JsonMessage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link ReadinessAwareRequestSender}.
 */
public final class ReadinessAwareRequestSenderTest {

    private FakeRequestObserver observer;

    private List<DynamicMessage> requests;

    @BeforeEach
    public void setUp() {
        observer = new FakeRequestObserver();
        requests = List.of(JsonMessage.buildJsonMessage(), JsonMessage.buildJsonMessage(), JsonMessage.buildJsonMessage());
    }

    @Test
    public void sendsAllMessagesWhenReady() {
        observer.setReady(true);
        ReadinessAwareRequestSender sender = new ReadinessAwareRequestSender(requests, observer);

        sender.start();

        assertEquals(requests, observer.getSent());
        assertEquals(1, observer.getCompleted());
        assertTrue(observer.isHandlerInstalledBeforeFirstSend());
    }

    @Test
    public void waitsForReadinessBeforeSending() {
        ReadinessAwareRequestSender sender = new ReadinessAwareRequestSender(requests, observer);

        sender.start();
        assertTrue(observer.getSent().isEmpty());
        assertEquals(0, observer.getCompleted());

        observer.setReady(true);
        observer.fireReady();

        assertEquals(requests, observer.getSent());
        assertEquals(1, observer.getCompleted());
    }

    @Test
    public void resumesAfterReadinessReturns() {
        AtomicInteger sent = new AtomicInteger();
        observer.setAfterNext(message -> {
            if (sent.incrementAndGet() == 1) {
                observer.setReady(false);
            }
        });
        observer.setReady(true);
        ReadinessAwareRequestSender sender = new ReadinessAwareRequestSender(requests, observer);

        sender.start();
        assertEquals(1, observer.getSent().size());

        observer.setReady(true);
        observer.fireReady();

        assertEquals(requests, observer.getSent());
        assertEquals(1, observer.getCompleted());
    }

    @Test
    public void reentrantReadinessCallbackDoesNotDuplicateMessages() {
        observer.setAfterNext(message -> observer.fireReady());
        observer.setReady(true);
        ReadinessAwareRequestSender sender = new ReadinessAwareRequestSender(requests, observer);

        sender.start();

        assertEquals(requests, observer.getSent());
        assertEquals(1, observer.getCompleted());
    }

    @Test
    public void terminationStopsPendingMessages() {
        observer.setAfterNext(message -> {
            observer.setReady(false);
            observer.fireReady();
        });
        observer.setReady(true);
        ReadinessAwareRequestSender sender = new ReadinessAwareRequestSender(requests, observer);
        sender.start();
        assertEquals(1, observer.getSent().size());

        sender.terminate();
        observer.setReady(true);
        observer.fireReady();

        assertEquals(1, observer.getSent().size());
        assertEquals(0, observer.getCompleted());
    }

    @Test
    public void terminationBeforeStartSendsNothing() {
        ReadinessAwareRequestSender sender = new ReadinessAwareRequestSender(requests, observer);

        sender.terminate();
        observer.setReady(true);
        sender.start();
        observer.fireReady();

        assertTrue(observer.getSent().isEmpty());
        assertEquals(0, observer.getCompleted());
    }

    @Test
    public void sendFailureCancelsTheStream() {
        observer.setFailOnSecondMessage(true);
        observer.setReady(true);
        ReadinessAwareRequestSender sender = new ReadinessAwareRequestSender(requests, observer);

        sender.start();

        assertEquals(1, observer.getSent().size());
        assertEquals(1, observer.getCancelled());
        assertEquals(0, observer.getCompleted());
        assertFalse(Objects.isNull(observer.getError()));
    }

    @Test
    public void emptyRequestListCompletesImmediately() {
        observer.setReady(true);
        ReadinessAwareRequestSender sender = new ReadinessAwareRequestSender(Collections.emptyList(), observer);

        sender.start();

        assertTrue(observer.getSent().isEmpty());
        assertEquals(1, observer.getCompleted());
    }

    private static final class FakeRequestObserver extends ClientCallStreamObserver<DynamicMessage> {

        private final List<DynamicMessage> sent = new ArrayList<>();

        private boolean ready;

        private boolean failOnSecondMessage;

        private boolean compressionEnabled;

        private int requested;

        private int autoInboundDisabled;

        private int completed;

        private int cancelled;

        private int handlerInstalls;

        private boolean handlerInstalledBeforeFirstSend;

        private Throwable error;

        private Runnable onReadyHandler;

        private Consumer<DynamicMessage> afterNext;

        @Override
        public void onNext(final DynamicMessage value) {
            if (failOnSecondMessage && sent.size() == 1) {
                throw new IllegalStateException("simulated send failure");
            }
            handlerInstalledBeforeFirstSend = handlerInstalls > 0;
            sent.add(value);
            if (Objects.nonNull(afterNext)) {
                afterNext.accept(value);
            }
        }

        @Override
        public void onError(final Throwable throwable) {
            this.error = throwable;
        }

        @Override
        public void onCompleted() {
            completed++;
        }

        @Override
        public void cancel(final String message, final Throwable cause) {
            cancelled++;
            this.error = cause;
        }

        @Override
        public boolean isReady() {
            return ready;
        }

        @Override
        public void setOnReadyHandler(final Runnable handler) {
            handlerInstalls++;
            this.onReadyHandler = handler;
        }

        @Override
        public void request(final int count) {
            this.requested = count;
        }

        @Override
        public void setMessageCompression(final boolean enable) {
            this.compressionEnabled = enable;
        }

        @Override
        public void disableAutoInboundFlowControl() {
            autoInboundDisabled++;
        }

        List<DynamicMessage> getSent() {
            return sent;
        }

        int getCompleted() {
            return completed;
        }

        int getCancelled() {
            return cancelled;
        }

        Throwable getError() {
            return error;
        }

        boolean isHandlerInstalledBeforeFirstSend() {
            return handlerInstalledBeforeFirstSend;
        }

        void setReady(final boolean ready) {
            this.ready = ready;
        }

        void setFailOnSecondMessage(final boolean failOnSecondMessage) {
            this.failOnSecondMessage = failOnSecondMessage;
        }

        void setAfterNext(final Consumer<DynamicMessage> afterNext) {
            this.afterNext = afterNext;
        }

        void fireReady() {
            if (Objects.nonNull(onReadyHandler)) {
                onReadyHandler.run();
            }
        }
    }
}
