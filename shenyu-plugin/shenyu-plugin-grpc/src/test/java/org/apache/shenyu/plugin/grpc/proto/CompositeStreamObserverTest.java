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
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.ExecutionException;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doAnswer;

/**
 * The Test Case For {@link CompositeStreamObserver}.
 */
@RunWith(MockitoJUnitRunner.class)
public class CompositeStreamObserverTest {

    /**
     * The Logger.
     */
    private Logger logger = LoggerFactory.getLogger(CompositeStreamObserverTest.class);

    private CompositeStreamObserver<Boolean> compositeStreamObserver;

    private CompleteObserver<Boolean> completeObserver;

    private MyStreamObserver streamObserver;

    @Before
    public void setUp() {
        streamObserver = new MyStreamObserver(false);
        completeObserver = new CompleteObserver<>();
        compositeStreamObserver = CompositeStreamObserver.of(streamObserver, completeObserver);
    }

    @Test
    public void onCompleted() throws ExecutionException, InterruptedException {
        CompositeStreamObserver compositeStreamObserverMock = mock(CompositeStreamObserver.class);
        doNothing().when(compositeStreamObserverMock).onCompleted();
        compositeStreamObserverMock.onCompleted();
        CompleteObserver completeObserverMock = mock(CompleteObserver.class);
        when(completeObserverMock.getCompletionFuture()).thenReturn(SettableFuture.create());
        ListenableFuture<Void> future = completeObserverMock.getCompletionFuture();
        future = mock(ListenableFuture.class);
        assert future.get() == null;
    }

    @Test(expected = Throwable.class)
    public void onError() throws Exception {
        Throwable throwable = new Throwable("error");
        CompleteObserver completeObserver = mock(CompleteObserver.class);
        doAnswer(invocationOnMock -> {
            logger.debug("test compositeStreamObserver onError");
            return null;
        }).when(completeObserver).onError(throwable);
        completeObserver.onError(throwable);
        completeObserver.getCompletionFuture().get();
    }

    @Test
    public void onNext() {
        compositeStreamObserver.onNext(true);
        assertTrue(streamObserver.getState());
    }

    @Test
    public void onNextThrowException() {
        CompleteObserver completeObserver = mock(CompleteObserver.class);
        doAnswer(invocationOnMock -> {
            logger.debug("test compositeStreamObserver onNext error");
            return null;
        }).when(completeObserver).onNext(Boolean.FALSE);
        completeObserver.onNext(Boolean.FALSE);
    }
}
