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

import com.google.protobuf.DynamicMessage;
import io.grpc.ClientCall;
import org.apache.shenyu.protocol.grpc.message.JsonMessage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * The Test Case For {@link MessageWriter}.
 */
@ExtendWith(MockitoExtension.class)
public class MessageWriterTest {

    private static DynamicMessage jsonMessage(final String payload) {
        return JsonMessage.buildJsonMessage(payload);
    }

    private static int bytes(final String payload) {
        return payload.getBytes(StandardCharsets.UTF_8).length;
    }

    @Test
    public void onError() {
        ShenyuGrpcResponse response = new ShenyuGrpcResponse();
        MessageWriter<DynamicMessage> messageWriter = MessageWriter.newInstance(response);
        messageWriter.onError(new Throwable());
    }

    @Test
    public void onCompleted() {
        ShenyuGrpcResponse response = new ShenyuGrpcResponse();
        MessageWriter<DynamicMessage> messageWriter = MessageWriter.newInstance(response);
        messageWriter.onCompleted();
    }

    @Test
    @SuppressWarnings("unchecked")
    public void unlimitedByDefaultRetainsAllResults() {
        ShenyuGrpcResponse response = new ShenyuGrpcResponse();
        MessageWriter<DynamicMessage> messageWriter = MessageWriter.newInstance(response);
        ClientCall<?, ?> call = mock(ClientCall.class);
        messageWriter.bindCall(call);

        for (int i = 0; i < 100; i++) {
            messageWriter.onNext(jsonMessage("{\"i\":\"" + i + "\"}"));
        }

        assertEquals(100, response.getResults().size());
        verify(call, never()).cancel(anyString(), any());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void withinCountBudgetDoesNotCancel() {
        ShenyuGrpcResponse response = new ShenyuGrpcResponse();
        MessageWriter<DynamicMessage> messageWriter = MessageWriter.newInstance(response, 3L, 0L);
        ClientCall<?, ?> call = mock(ClientCall.class);
        messageWriter.bindCall(call);

        messageWriter.onNext(jsonMessage("{\"a\":\"1\"}"));
        messageWriter.onNext(jsonMessage("{\"a\":\"2\"}"));
        messageWriter.onNext(jsonMessage("{\"a\":\"3\"}"));

        assertEquals(3, response.getResults().size());
        verify(call, never()).cancel(anyString(), any());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void exceedingCountBudgetCancelsAndReleasesResults() {
        ShenyuGrpcResponse response = new ShenyuGrpcResponse();
        MessageWriter<DynamicMessage> messageWriter = MessageWriter.newInstance(response, 2L, 0L);
        ClientCall<?, ?> call = mock(ClientCall.class);
        messageWriter.bindCall(call);

        messageWriter.onNext(jsonMessage("{\"a\":\"1\"}"));
        messageWriter.onNext(jsonMessage("{\"a\":\"2\"}"));
        // this one crosses the count budget.
        messageWriter.onNext(jsonMessage("{\"a\":\"3\"}"));

        assertTrue(response.getResults().isEmpty());
        verify(call, times(1)).cancel(anyString(), any());

        // subsequent messages are ignored and do not cancel again.
        messageWriter.onNext(jsonMessage("{\"a\":\"4\"}"));
        assertTrue(response.getResults().isEmpty());
        verify(call, times(1)).cancel(anyString(), any());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void atByteBudgetDoesNotCancel() {
        String payload = "{\"a\":\"bb\"}";
        ShenyuGrpcResponse response = new ShenyuGrpcResponse();
        MessageWriter<DynamicMessage> messageWriter = MessageWriter.newInstance(response, 0L, bytes(payload));
        ClientCall<?, ?> call = mock(ClientCall.class);
        messageWriter.bindCall(call);

        messageWriter.onNext(jsonMessage(payload));

        assertEquals(1, response.getResults().size());
        verify(call, never()).cancel(anyString(), any());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void exceedingByteBudgetCancelsAndReleasesResults() {
        String payload = "{\"a\":\"bb\"}";
        ShenyuGrpcResponse response = new ShenyuGrpcResponse();
        MessageWriter<DynamicMessage> messageWriter = MessageWriter.newInstance(response, 0L, bytes(payload));
        ClientCall<?, ?> call = mock(ClientCall.class);
        messageWriter.bindCall(call);

        messageWriter.onNext(jsonMessage(payload));
        // accumulated bytes now exceed the budget.
        messageWriter.onNext(jsonMessage(payload));

        assertTrue(response.getResults().isEmpty());
        verify(call, times(1)).cancel(anyString(), any());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void overflowDoesNotCancelWhenCallNotBound() {
        ShenyuGrpcResponse response = new ShenyuGrpcResponse();
        MessageWriter<DynamicMessage> messageWriter = MessageWriter.newInstance(response, 1L, 0L);

        messageWriter.onNext(jsonMessage("{\"a\":\"1\"}"));
        messageWriter.onNext(jsonMessage("{\"a\":\"2\"}"));

        // results are still released even without a bound call; no exception is thrown.
        assertTrue(response.getResults().isEmpty());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void concurrentWritersAreIsolated() {
        ShenyuGrpcResponse responseA = new ShenyuGrpcResponse();
        ShenyuGrpcResponse responseB = new ShenyuGrpcResponse();
        MessageWriter<DynamicMessage> writerA = MessageWriter.newInstance(responseA, 1L, 0L);
        MessageWriter<DynamicMessage> writerB = MessageWriter.newInstance(responseB, 0L, 0L);
        ClientCall<?, ?> callA = mock(ClientCall.class);
        ClientCall<?, ?> callB = mock(ClientCall.class);
        writerA.bindCall(callA);
        writerB.bindCall(callB);

        // writer A overflows, writer B keeps accumulating.
        writerA.onNext(jsonMessage("{\"a\":\"1\"}"));
        writerA.onNext(jsonMessage("{\"a\":\"2\"}"));
        writerB.onNext(jsonMessage("{\"b\":\"1\"}"));
        writerB.onNext(jsonMessage("{\"b\":\"2\"}"));

        assertTrue(responseA.getResults().isEmpty());
        verify(callA, times(1)).cancel(anyString(), any());
        assertEquals(2, responseB.getResults().size());
        verify(callB, never()).cancel(anyString(), any());
    }
}
