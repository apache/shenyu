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
import co.elastic.clients.elasticsearch._types.ElasticsearchException;
import co.elastic.clients.elasticsearch._types.ErrorResponse;
import co.elastic.clients.elasticsearch.indices.ElasticsearchIndicesClient;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.elasticsearch.config.ElasticSearchLogCollectConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * test cases for ElasticSearchLog.
 */
public class ElasticSearchLogCollectClientTest {

    private ElasticSearchLogCollectClient elasticSearchLogCollectClient;

    private final PluginData pluginData = new PluginData();

    private ElasticSearchLogCollectConfig.ElasticSearchLogConfig elasticSearchLogConfig;

    private final List<ShenyuRequestLog> logs = new ArrayList<>();

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    @BeforeEach
    public void setUp() {
        this.elasticSearchLogCollectClient = new ElasticSearchLogCollectClient();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"host\":\"localhost\", \"port\":\"9200\", \"userName\": \"shenyu\",\"password\": \"shenyu\", \"authCache\": \"true\"}");
        elasticSearchLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(),
                ElasticSearchLogCollectConfig.ElasticSearchLogConfig.class);
        
        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
        logs.add(shenyuRequestLog);
    }

    @Test
    public void testConsume() {
        String msg = "";
        ElasticSearchLogCollectConfig.INSTANCE.setElasticSearchLogConfig(elasticSearchLogConfig);
        elasticSearchLogCollectClient.initClient(elasticSearchLogConfig);
        try {
            elasticSearchLogCollectClient.consume(logs);
        } catch (Exception e) {
            msg = "false";
        }
        Assertions.assertEquals(msg, "");
        elasticSearchLogCollectClient.close();
    }

    @Test
    public void testCreateIndex() {
        ElasticSearchLogCollectConfig.INSTANCE.setElasticSearchLogConfig(elasticSearchLogConfig);
        elasticSearchLogCollectClient.initClient(elasticSearchLogConfig);
        elasticSearchLogCollectClient.createIndex("test");
    }

    @Test
    public void testExistsIndexReturnsFalseWhenCheckFails() throws Exception {
        ElasticsearchClient client = mock(ElasticsearchClient.class);
        ElasticsearchIndicesClient indicesClient = mock(ElasticsearchIndicesClient.class);
        when(client.indices()).thenReturn(indicesClient);
        when(indicesClient.exists(any(Function.class))).thenThrow(new IOException("connection failed"));
        setClient(client);

        assertFalse(elasticSearchLogCollectClient.existsIndex("missing-index"));
    }

    @Test
    public void testCreateIndexIgnoresAlreadyExistsException() throws Exception {
        ElasticsearchIndicesClient indicesClient = mock(ElasticsearchIndicesClient.class);
        ElasticsearchException alreadyExists = elasticsearchException("resource_already_exists_exception");
        when(indicesClient.create(any(Function.class))).thenThrow(alreadyExists);
        ElasticsearchClient client = mock(ElasticsearchClient.class);
        when(client.indices()).thenReturn(indicesClient);
        setClient(client);

        assertDoesNotThrow(() -> elasticSearchLogCollectClient.createIndex("existing-index"));
    }

    @Test
    public void testCreateIndexRethrowsOtherElasticsearchExceptions() throws Exception {
        ElasticsearchIndicesClient indicesClient = mock(ElasticsearchIndicesClient.class);
        ElasticsearchException unavailable = elasticsearchException("cluster_block_exception");
        when(indicesClient.create(any(Function.class))).thenThrow(unavailable);
        ElasticsearchClient client = mock(ElasticsearchClient.class);
        when(client.indices()).thenReturn(indicesClient);
        setClient(client);

        assertThrows(ElasticsearchException.class,
                () -> elasticSearchLogCollectClient.createIndex("blocked-index"));
    }

    private ElasticsearchException elasticsearchException(final String type) {
        ErrorResponse response = ErrorResponse.of(builder -> builder
                .status(400)
                .error(error -> error.type(type).reason("test")));
        return new ElasticsearchException("indices.create", response);
    }

    private void setClient(final ElasticsearchClient client) throws ReflectiveOperationException {
        Field clientField = ElasticSearchLogCollectClient.class.getDeclaredField("client");
        clientField.setAccessible(true);
        clientField.set(elasticSearchLogCollectClient, client);
    }
}
