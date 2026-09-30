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

package org.apache.shenyu.plugin.mcp.server.transport;

import io.modelcontextprotocol.json.McpJsonMapper;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Test cases for MessageHandlingResult.
 */
public final class MessageHandlingResultTest {

    @Test
    public void testConfiguredMapperSerializesResponseBody() throws Exception {
        McpJsonMapper jsonMapper = mock(McpJsonMapper.class);
        Object responseBody = new Object();
        when(jsonMapper.writeValueAsString(responseBody)).thenReturn("{\"jsonrpc\":\"2.0\"}");
        MessageHandlingResult result = new MessageHandlingResult(200, responseBody, "session", jsonMapper);

        assertEquals("{\"jsonrpc\":\"2.0\"}", result.getResponseBodyAsJson());
        verify(jsonMapper).writeValueAsString(responseBody);
    }

    @Test
    public void testStringResponseBodyDoesNotRequireSerialization() {
        McpJsonMapper jsonMapper = mock(McpJsonMapper.class);
        MessageHandlingResult result = new MessageHandlingResult(200, "response", "session", jsonMapper);

        assertEquals("response", result.getResponseBodyAsJson());
        verifyNoInteractions(jsonMapper);
    }
}
