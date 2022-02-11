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

import com.google.protobuf.Descriptors;
import com.google.protobuf.DynamicMessage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DynamicMessageMarshallerTest {
    
    private DynamicMessageMarshaller dynamicMessageMarshaller;

    @BeforeEach
    public void setUp() {
        Descriptors.Descriptor messageDescriptor = mock(Descriptors.Descriptor.class, RETURNS_DEEP_STUBS);
        when(messageDescriptor.toProto().getOneofDeclCount()).thenReturn(2);
        when(messageDescriptor.getOptions().getMapEntry()).thenReturn(true);
        dynamicMessageMarshaller = new DynamicMessageMarshaller(messageDescriptor);
    }

    @Test
    public void testParse() {
        InputStream inputStream = new ByteArrayInputStream("".getBytes());
        final DynamicMessage parse = dynamicMessageMarshaller.parse(inputStream);
        assertNotNull(parse);
    }

    @Test
    public void testParseThrowException() {
        InputStream inputStream = new ByteArrayInputStream("test".getBytes());
        assertThrows(RuntimeException.class, () -> {
            dynamicMessageMarshaller.parse(inputStream);
        });
    }

    @Test
    public void testStream() {
        DynamicMessage dynamicMessage = mock(DynamicMessage.class, RETURNS_DEEP_STUBS);
        InputStream inputStream = mock(InputStream.class);
        when(dynamicMessage.toByteString().newInput()).thenReturn(inputStream);
        InputStream actualInputStream = dynamicMessageMarshaller.stream(dynamicMessage);
        assertEquals(inputStream, actualInputStream);
    }
}
