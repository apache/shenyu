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


package org.apache.shenyu.plugin.logging.elasticsearch.client;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import co.elastic.clients.elasticsearch.indices.CreateIndexRequest;
import co.elastic.clients.elasticsearch.indices.ElasticsearchIndicesClient;
import co.elastic.clients.util.ObjectBuilder;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class ElasticSearchLogMappingTest {

    @Test
    void createsDailyIndicesWithExplicitLogFieldTypes() throws Exception {
        ElasticsearchClient client = mock(ElasticsearchClient.class);
        ElasticsearchIndicesClient indices = mock(ElasticsearchIndicesClient.class);
        when(client.indices()).thenReturn(indices);
        doAnswer(invocation -> {
            Function<CreateIndexRequest.Builder, ObjectBuilder<CreateIndexRequest>> factory = invocation.getArgument(0);
            CreateIndexRequest request = factory.apply(new CreateIndexRequest.Builder()).build();
            assertEquals("logs-2026-09-25", request.index());
            assertTrue(request.mappings().properties().get("timeLocal").isDate());
            assertEquals("yyyy-MM-dd HH:mm:ss.SSS", request.mappings().properties().get("timeLocal").date().format());
            assertTrue(request.mappings().properties().get("responseContentLength").isInteger());
            assertTrue(request.mappings().properties().get("status").isInteger());
            assertTrue(request.mappings().properties().get("upstreamResponseTime").isLong());
            return null;
        }).when(indices).create(any(Function.class));
        ElasticSearchLogCollectClient collector = new ElasticSearchLogCollectClient();
        ReflectionTestUtils.setField(collector, "client", client);
        collector.createIndex("logs-2026-09-25");
        verify(indices).create(any(Function.class));
    }
}
