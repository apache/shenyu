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

import com.google.protobuf.InvalidProtocolBufferException;
import com.google.protobuf.Message;
import com.google.protobuf.util.JsonFormat;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import java.lang.reflect.Field;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For {@link MessageWriter}.
 */
@RunWith(MockitoJUnitRunner.class)
public class MessageWriterTest {

    private JsonFormat.TypeRegistry registry;

    private ShenyuGrpcResponse shenyuGrpcResponse;

    private MessageWriter<Message> messageWriter;

    @Before
    public void setUp() {
        registry = mock(JsonFormat.TypeRegistry.class);
        shenyuGrpcResponse = mock(ShenyuGrpcResponse.class);
        messageWriter = MessageWriter.newInstance(registry, shenyuGrpcResponse);
    }

    @Test
    public void testNewInstance() {
        assertNotNull(MessageWriter.newInstance(registry, shenyuGrpcResponse));
    }

    @Test
    public void testOnNext() throws Exception {
        Message value = mock(Message.class);
        Field field = messageWriter.getClass().getDeclaredField("printer");
        field.setAccessible(true);
        JsonFormat.Printer printer = mock(JsonFormat.Printer.class);
        field.set(messageWriter, printer);
        when(printer.print(value)).thenReturn("grpc");
        messageWriter.onNext(value);
    }

    @Test
    public void testOnNextThrowException() throws Exception {
        Message value = mock(Message.class);
        Field field = messageWriter.getClass().getDeclaredField("printer");
        field.setAccessible(true);
        JsonFormat.Printer printer = mock(JsonFormat.Printer.class);
        field.set(messageWriter, printer);
        when(printer.print(value)).thenThrow(InvalidProtocolBufferException.class);
        messageWriter.onNext(value);
    }

    @Test
    public void onError() {
        Throwable throwable = mock(Throwable.class);
        messageWriter.onError(throwable);
    }

    @Test
    public void onCompleted() {
        messageWriter.onCompleted();
    }
}
