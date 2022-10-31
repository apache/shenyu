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

import com.google.common.collect.ImmutableList;
import io.grpc.stub.StreamObserver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A which groups multiple observers and executes them all.
 */
public final class CompositeStreamObserver<T> implements StreamObserver<T> {

    private static final Logger LOG = LoggerFactory.getLogger(CompositeStreamObserver.class);

    private final ImmutableList<StreamObserver<T>> observers;

    private CompositeStreamObserver(final ImmutableList<StreamObserver<T>> observers) {
        this.observers = observers;
    }

    /**
     * CompositeStreamObserver of.
     *
     * @param observers        observers
     * @param completeObserver completeObserver
     * @param <T>              t
     * @return CompositeStreamObserver compositeStreamObserver
     */
    public static <T> CompositeStreamObserver<T> of(final StreamObserver<T> observers,
                                                    final CompleteObserver<T> completeObserver) {
        return new CompositeStreamObserver<>(ImmutableList.of(observers, completeObserver));
    }

    @Override
    public void onCompleted() {
        for (StreamObserver<T> observer : observers) {
            try {
                observer.onCompleted();
            } catch (Exception t) {
                LOG.error("Exception in composite onComplete, moving on", t);
            }
        }
    }

    @Override
    public void onError(final Throwable t) {
        for (StreamObserver<T> observer : observers) {
            try {
                observer.onError(t);
            } catch (Exception exception) {
                LOG.error("Exception in composite onError, moving on", exception);
            }
        }
    }

    @Override
    public void onNext(final T value) {
        for (StreamObserver<T> observer : observers) {
            try {
                observer.onNext(value);
            } catch (Exception exception) {
                LOG.error("Exception in composite onNext, moving on", exception);
            }
        }
    }
}
