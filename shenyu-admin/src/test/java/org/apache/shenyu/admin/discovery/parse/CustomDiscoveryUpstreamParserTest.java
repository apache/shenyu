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

package org.apache.shenyu.admin.discovery.parse;

import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class CustomDiscoveryUpstreamParserTest {

    @InjectMocks
    private CustomDiscoveryUpstreamParser customDiscoveryUpstreamParser;

    @BeforeEach
    void setUp() {
        Map<String, String> conversion = new HashMap<>();
        customDiscoveryUpstreamParser = new CustomDiscoveryUpstreamParser(conversion);
    }

    @Test
    void testParseValue() {
        final String jsonString = "{\"protocol\":\"tcp\",\"url\":\"127.0.0.1:8188\",\"status\":1,\"weight\":1}";
        final List<DiscoveryUpstreamData> expectDiscoveryUpstreamData = new ArrayList<>();
        DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
        discoveryUpstreamData.setUrl("127.0.0.1:8188");
        discoveryUpstreamData.setProtocol("tcp");
        discoveryUpstreamData.setStatus(1);
        discoveryUpstreamData.setWeight(1);
        expectDiscoveryUpstreamData.add(discoveryUpstreamData);
        List<DiscoveryUpstreamData> actualDiscoveryUpstreamData = customDiscoveryUpstreamParser.parseValue(jsonString);
        assertEquals(expectDiscoveryUpstreamData, actualDiscoveryUpstreamData);

    }
}
