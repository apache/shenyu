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
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class DynamicMessageMarshallerTest {

    private Descriptors.Descriptor messageDescriptor;

    private DynamicMessageMarshaller dynamicMessageMarshaller;

    @Before
    public void setUp() {
        messageDescriptor = mock(Descriptors.Descriptor.class, RETURNS_DEEP_STUBS);
        when(messageDescriptor.toProto().getOneofDeclCount()).thenReturn(2);
        when(messageDescriptor.getOptions().getMapEntry()).thenReturn(true);
        dynamicMessageMarshaller = new DynamicMessageMarshaller(messageDescriptor);
    }

    @Test
    public void testParse() {
        InputStream inputStream = new ByteArrayInputStream("".getBytes());
        dynamicMessageMarshaller.parse(inputStream);
    }

    @Test(expected = RuntimeException.class)
    public void testParseThrowException() {
        InputStream inputStream = new ByteArrayInputStream("test".getBytes());
        dynamicMessageMarshaller.parse(inputStream);
    }

    @Test
    public void testStream() {
        DynamicMessage dynamicMessage = mock(DynamicMessage.class, RETURNS_DEEP_STUBS);
        InputStream inputStream = mock(InputStream.class);
        when(dynamicMessage.toByteString().newInput()).thenReturn(inputStream);
        InputStream actualInputStream = dynamicMessageMarshaller.stream(dynamicMessage);
        Assert.assertEquals(inputStream, actualInputStream);
    }
}
