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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.ExecutionException;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doAnswer;

/**
 * The Test Case For {@link CompositeStreamObserver}.
 */
@ExtendWith(MockitoExtension.class)
public class CompositeStreamObserverTest {

    /**
     * The Logger.
     */
    private final Logger logger = LoggerFactory.getLogger(CompositeStreamObserverTest.class);

    private CompositeStreamObserver<Boolean> compositeStreamObserver;
    
    private MyStreamObserver streamObserver;

    @BeforeEach
    public void setUp() {
        streamObserver = new MyStreamObserver(false);
        CompleteObserver<Boolean> completeObserver = new CompleteObserver<>();
        compositeStreamObserver = CompositeStreamObserver.of(streamObserver, completeObserver);
    }

    @Test
    @SuppressWarnings("all")
    public void onCompleted() throws ExecutionException, InterruptedException {
        CompositeStreamObserver compositeStreamObserverMock = mock(CompositeStreamObserver.class);
        doNothing().when(compositeStreamObserverMock).onCompleted();
        compositeStreamObserverMock.onCompleted();
        CompleteObserver completeObserverMock = mock(CompleteObserver.class);
        when(completeObserverMock.getCompletionFuture()).thenReturn(SettableFuture.create());
        ListenableFuture future = completeObserverMock.getCompletionFuture();
        future = mock(ListenableFuture.class);
        assertNull(future.get());
    }

    @Test
    @SuppressWarnings("all")
    public void onError() throws Exception {
        Throwable throwable = new Throwable("error");
        CompleteObserver completeObserver = mock(CompleteObserver.class);
        doAnswer(invocationOnMock -> {
            logger.debug("test compositeStreamObserver onError");
            return null;
        }).when(completeObserver).onError(throwable);
        completeObserver.onError(throwable);
        assertThrows(Throwable.class, () -> {
            completeObserver.getCompletionFuture().get();
        });
    }

    @Test
    public void onNext() {
        compositeStreamObserver.onNext(true);
        assertTrue(streamObserver.getState());
    }

    @Test
    @SuppressWarnings("all")
    public void onNextThrowException() {
        CompleteObserver completeObserver = mock(CompleteObserver.class);
        doAnswer(invocationOnMock -> {
            logger.debug("test compositeStreamObserver onNext error");
            return null;
        }).when(completeObserver).onNext(Boolean.FALSE);
        completeObserver.onNext(Boolean.FALSE);
    }
}
