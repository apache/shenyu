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

import com.google.protobuf.Message;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.mock;

/**
 * The Test Case For {@link MessageWriter}.
 */
@ExtendWith(MockitoExtension.class)
public class MessageWriterTest {
    
    private MessageWriter<Message> messageWriter;

    @BeforeEach
    public void setUp() {
        ShenyuGrpcResponse shenyuGrpcResponse = mock(ShenyuGrpcResponse.class);
        messageWriter = MessageWriter.newInstance(shenyuGrpcResponse);
    }

    @Test
    public void onError() {
        Throwable throwable = new Throwable();
        messageWriter.onError(throwable);
        
    }

    @Test
    public void onCompleted() {
        messageWriter.onCompleted();
    }
}
