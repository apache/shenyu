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

package org.apache.shenyu.plugin.ai.api;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.plugin.ai.api.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.api.spi.ShenyuAiProtocol;
import org.apache.shenyu.plugin.ai.api.spi.ShenyuAiProvider;
import org.apache.shenyu.plugin.ai.api.spi.ShenyuAiTransport;
import org.apache.shenyu.spi.SPI;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Tests the shared AI API serialization and extension contracts.
 */
public final class ShenyuAiApiContractTest {

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Test
    public void testRequestRetainsUnknownProtocolFields() throws Exception {
        JsonNode payload = objectMapper.readTree("{\"model\":\"m1\",\"vendor_option\":{\"mode\":\"fast\"}}");
        ShenyuAiRequest request = new ShenyuAiRequest("chat", "m1", false, null, payload);

        ShenyuAiRequest restored = objectMapper.readValue(objectMapper.writeValueAsBytes(request), ShenyuAiRequest.class);

        assertEquals("fast", restored.payload().path("vendor_option").path("mode").asText());
        assertEquals("m1", restored.model());
    }

    @Test
    public void testExtensionContractsAreShenyuSpiInterfaces() {
        assertNotNull(ShenyuAiProtocol.class.getAnnotation(SPI.class));
        assertNotNull(ShenyuAiProvider.class.getAnnotation(SPI.class));
        assertNotNull(ShenyuAiTransport.class.getAnnotation(SPI.class));
    }
}
