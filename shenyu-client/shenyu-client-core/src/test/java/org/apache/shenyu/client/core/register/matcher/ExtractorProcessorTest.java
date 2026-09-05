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

package org.apache.shenyu.client.core.register.matcher;

import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ExtractorProcessorTest {

    private final ExtractorProcessor extractorProcessor = new ExtractorProcessor() {

        @Override
        public void process(final ApiBean apiBean) {
        }

        @Override
        public void process(final ApiBean.ApiDefinition apiDefinition) {
        }
    };

    @Test
    public void testSupportedClientContainsAllRpcTypes() {
        List<String> expected = Arrays.stream(RpcTypeEnum.values())
                .map(RpcTypeEnum::getName)
                .collect(Collectors.toList());
        List<String> actual = extractorProcessor.supportedClient();
        assertEquals(expected, actual);
    }

    @Test
    public void testSupportedClientNotEmpty() {
        List<String> supportedClient = extractorProcessor.supportedClient();
        assertNotNull(supportedClient);
        assertTrue(supportedClient.contains(RpcTypeEnum.HTTP.getName()));
    }
}
