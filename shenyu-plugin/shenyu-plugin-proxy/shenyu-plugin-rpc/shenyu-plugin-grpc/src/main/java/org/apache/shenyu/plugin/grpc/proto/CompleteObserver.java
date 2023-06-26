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

package org.apache.shenyu.plugin.grpc.proto;

import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.SettableFuture;
import io.grpc.stub.StreamObserver;

/**
 * A holding a future which completes when the rpc terminates.
 */
public class CompleteObserver<T> implements StreamObserver<T> {

    private final SettableFuture<Void> doneFuture;

    public CompleteObserver() {
        this.doneFuture = SettableFuture.create();
    }

    @Override
    public synchronized void onCompleted() {
        doneFuture.set(null);
    }

    @Override
    public synchronized void onError(final Throwable t) {
        doneFuture.setException(t);
    }

    @Override
    public void onNext(final T next) {
    }

    /**
     * Returns a future which completes when the rpc finishes.
     * The returned future fails if the rpc fails.
     *
     * @return ListenableFuture future
     */
    public ListenableFuture<Void> getCompletionFuture() {
        return doneFuture;
    }
}
