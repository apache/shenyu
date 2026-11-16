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
import io.grpc.stub.ClientResponseObserver;
import io.grpc.stub.StreamObserver;

import java.util.List;
import java.util.Objects;

/**
 * Bridges a streaming response observer and a readiness-aware request sender.
 */
final class StreamingResponseObserver implements ClientResponseObserver<DynamicMessage, DynamicMessage> {

    private final StreamObserver<DynamicMessage> delegate;

    private final List<DynamicMessage> requests;

    private volatile ReadinessAwareRequestSender sender;

    StreamingResponseObserver(final StreamObserver<DynamicMessage> delegate, final List<DynamicMessage> requests) {
        this.delegate = delegate;
        this.requests = requests;
    }

    @Override
    public void beforeStart(final ClientCallStreamObserver<DynamicMessage> requestStream) {
        // beforeStart runs before gRPC freezes the stream, so the readiness handler can be installed here.
        this.sender = new ReadinessAwareRequestSender(requests, requestStream);
    }

    boolean start() {
        if (Objects.isNull(sender)) {
            return false;
        }
        sender.start();
        return true;
    }

    void terminate() {
        if (Objects.nonNull(sender)) {
            sender.terminate();
        }
    }

    @Override
    public void onNext(final DynamicMessage value) {
        delegate.onNext(value);
    }

    @Override
    public void onError(final Throwable throwable) {
        terminate();
        delegate.onError(throwable);
    }

    @Override
    public void onCompleted() {
        terminate();
        delegate.onCompleted();
    }
}
