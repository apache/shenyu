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

package org.apache.shenyu.protocol.grpc.message;

import com.google.protobuf.Descriptors;
import com.google.protobuf.DynamicMessage;
import io.grpc.MethodDescriptor;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.protocol.grpc.constant.GrpcConstants;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.List;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;

/**
 * The Test Case For {@link JsonMessage}.
 */
@RunWith(MockitoJUnitRunner.class)
public class JsonMessageTest {

    @Test
    public void testBuildJsonMessage() {
        DynamicMessage jsonMessage = JsonMessage.buildJsonMessage();

        assertEquals(GrpcConstants.JSON_DESCRIPTOR_PROTO_NAME, jsonMessage.getDescriptorForType().getFullName());
    }

    @Test
    public void testBuildJsonMessageWithParam() {
        String jsonParam = "{\"text\":\"hello world\"}";

        DynamicMessage jsonMessage = JsonMessage.buildJsonMessage(jsonParam);
        assertEquals(GrpcConstants.JSON_DESCRIPTOR_PROTO_NAME, jsonMessage.getDescriptorForType().getFullName());

        Descriptors.FieldDescriptor fieldDescriptor = jsonMessage.getDescriptorForType().findFieldByName(GrpcConstants.JSON_DESCRIPTOR_PROTO_FIELD_NAME);
        assertTrue(jsonMessage.hasField(fieldDescriptor));

        String field = (String) jsonMessage.getField(fieldDescriptor);
        assertEquals(jsonParam, field);
    }

    @Test
    public void testBuildJsonMessageList() {
        String jsonParam = "{\"data\":[{\"text\":\"hello\"}, {\"text\":\"world\"}]}\n";

        List<DynamicMessage> jsonMessageList = JsonMessage.buildJsonMessageList(GsonUtils.getInstance().toObjectMap(jsonParam));
        assertEquals(2, jsonMessageList.size());

        DynamicMessage jsonMessage = jsonMessageList.get(0);
        assertEquals(GrpcConstants.JSON_DESCRIPTOR_PROTO_NAME, jsonMessage.getDescriptorForType().getFullName());

        Descriptors.FieldDescriptor fieldDescriptor = jsonMessage.getDescriptorForType().findFieldByName(GrpcConstants.JSON_DESCRIPTOR_PROTO_FIELD_NAME);
        assertTrue(jsonMessage.hasField(fieldDescriptor));

        String field = (String) jsonMessage.getField(fieldDescriptor);
        assertEquals("{\"text\":\"hello\"}", field);

        jsonMessage = jsonMessageList.get(1);
        assertEquals(GrpcConstants.JSON_DESCRIPTOR_PROTO_NAME, jsonMessage.getDescriptorForType().getFullName());

        fieldDescriptor = jsonMessage.getDescriptorForType().findFieldByName(GrpcConstants.JSON_DESCRIPTOR_PROTO_FIELD_NAME);
        assertTrue(jsonMessage.hasField(fieldDescriptor));

        field = (String) jsonMessage.getField(fieldDescriptor);
        assertEquals("{\"text\":\"world\"}", field);
    }

    @Test
    public void testGetDataFromDynamicMessage() {
        String jsonParam = "{\"text\":\"hello world\"}";

        DynamicMessage jsonMessage = JsonMessage.buildJsonMessage(jsonParam);
        String data = JsonMessage.getDataFromDynamicMessage(jsonMessage);
        assertEquals(jsonParam, data);
    }

    @Test
    public void testCreateJsonMarshallerMethodDescriptor() {
        DynamicMessage jsonMessage = JsonMessage.buildJsonMessage();
        MethodDescriptor<DynamicMessage, DynamicMessage> echo = JsonMessage.createJsonMarshallerMethodDescriptor("echo.service",
                "echo",
                MethodDescriptor.MethodType.UNARY,
                jsonMessage,
                jsonMessage);

        assertEquals("echo.service" + GrpcConstants.GRPC_JSON_SERVICE + "/echo", echo.getFullMethodName());
        assertEquals(MethodDescriptor.MethodType.UNARY, echo.getType());
        assertFalse(echo.isIdempotent());
        assertFalse(echo.isSafe());
    }

    @Test
    public void testParseOfDynamicMessageMarshaller() {
        String jsonParam = "{\"text\":\"hello world\"}";
        DynamicMessage jsonMessage = JsonMessage.buildJsonMessage(jsonParam);
        MethodDescriptor<DynamicMessage, DynamicMessage> echo = JsonMessage.createJsonMarshallerMethodDescriptor("echo.service",
                "echo",
                MethodDescriptor.MethodType.BIDI_STREAMING,
                jsonMessage,
                jsonMessage);

        MethodDescriptor.Marshaller<DynamicMessage> requestMarshaller = echo.getRequestMarshaller();

        InputStream inputStream = new ByteArrayInputStream("".getBytes());
        requestMarshaller.parse(inputStream);
    }

    @Test
    public void testStreamOfDynamicMessageMarshaller() {
        DynamicMessage dynamicMessage = mock(DynamicMessage.class, RETURNS_DEEP_STUBS);
        InputStream inputStream = mock(InputStream.class);
        when(dynamicMessage.toByteString().newInput()).thenReturn(inputStream);

        String jsonParam = "{\"text\":\"hello world\"}";
        DynamicMessage jsonMessage = JsonMessage.buildJsonMessage(jsonParam);
        MethodDescriptor<DynamicMessage, DynamicMessage> echo = JsonMessage.createJsonMarshallerMethodDescriptor("echo.service",
                "echo",
                MethodDescriptor.MethodType.CLIENT_STREAMING,
                jsonMessage,
                jsonMessage);

        MethodDescriptor.Marshaller<DynamicMessage> requestMarshaller = echo.getRequestMarshaller();

        InputStream actualInputStream = requestMarshaller.stream(dynamicMessage);
        Assert.assertEquals(inputStream, actualInputStream);
    }
}
