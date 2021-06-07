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
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.concurrent.ExecutionException;

/**
 * The Test Case For {@link CompleteObserver}.
 */
@RunWith(MockitoJUnitRunner.class)
public class CompleteObserverTest {

    private CompleteObserver completeObserver;

    @Before
    public void setUp() {
        completeObserver = new CompleteObserver();
    }

    @Test
    public void onCompleted() throws ExecutionException, InterruptedException {
        completeObserver.onCompleted();
        ListenableFuture future = completeObserver.getCompletionFuture();
        assert future.get() == null;
    }

    @Test(expected = Throwable.class)
    public void onError() throws Exception {
        Throwable throwable = new Throwable("error");
        completeObserver.onError(throwable);
        ListenableFuture future = completeObserver.getCompletionFuture();
        future.get();
    }

    @Test
    public void onNext() {
        completeObserver.onNext(new Object());
    }
}
