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

package org.apache.shenyu.plugin.logging.elasticsearch.elasticsearch;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import co.elastic.clients.elasticsearch.core.BulkResponse;
import co.elastic.clients.elasticsearch.core.bulk.BulkOperation;
import co.elastic.clients.elasticsearch.indices.CreateIndexResponse;
import co.elastic.clients.json.jackson.JacksonJsonpMapper;
import co.elastic.clients.transport.ElasticsearchTransport;
import co.elastic.clients.transport.endpoints.BooleanResponse;
import co.elastic.clients.transport.rest_client.RestClientTransport;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.http.HttpHost;
import org.apache.shenyu.plugin.logging.elasticsearch.LogConsumeClient;
import org.apache.shenyu.plugin.logging.elasticsearch.constant.LoggingConstant;
import org.apache.shenyu.plugin.logging.elasticsearch.entity.ShenyuRequestLog;
import org.elasticsearch.client.RestClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * queue-based logging collector.
 */
public class ElasticSearchLogCollectClient implements LogConsumeClient {

    private static final Logger LOG = LoggerFactory.getLogger(ElasticSearchLogCollectClient.class);

    private RestClient restClient;

    private ElasticsearchTransport transport;

    private ElasticsearchClient client;

    private final AtomicBoolean isStarted = new AtomicBoolean(false);

    /**
     * init elasticsearch client.
     *
     * @param props elasticsearch client properties
     */
    public void initClient(final Properties props) {
        restClient = RestClient
                .builder(new HttpHost(props.getProperty(LoggingConstant.HOST), Integer.parseInt(props.getProperty(LoggingConstant.PORT))))
                .build();
        transport = new RestClientTransport(restClient, new JacksonJsonpMapper());
        client = new ElasticsearchClient(transport);
        LOG.info("init ElasticSearchLogCollectClient success");
        isStarted.set(true);
        Runtime.getRuntime().addShutdownHook(new Thread(this::close));
    }

    @Override
    public void consume(final List<ShenyuRequestLog> logs) throws Exception {
        if (!existsIndex(LoggingConstant.INDEX)) {
            createIndex(LoggingConstant.INDEX);
        }
        if (CollectionUtils.isEmpty(logs) || !isStarted.get()) {
            return;
        }
        List<BulkOperation> bulkOperations = new ArrayList<>();
        logs.forEach(log -> {
            try {
                bulkOperations.add(new BulkOperation.Builder().create(d -> d.document(log).index(LoggingConstant.INDEX)).build());
            } catch (Exception e) {
                LOG.error("elasticsearch store logs error", e);
            }
        });
        BulkResponse response = client.bulk(e -> e.index(LoggingConstant.INDEX).operations(bulkOperations));
    }

    /**
     * determine whether the index already exists.
     *
     * @param indexName index name
     * @return true or false
     */
    public boolean existsIndex(final String indexName) {
        try {
            BooleanResponse existsResponse = client.indices().exists(c -> c.index(indexName));
            return existsResponse.value();
        } catch (Exception e) {
            LOG.error("fail to check the index exists");
        }
        return true;
    }

    /**
     * create elasticsearch client.
     *
     * @param indexName index name
     * @return true or false
     */
    public boolean createIndex(final String indexName) {
        try {
            CreateIndexResponse createIndexResponse = client.indices().create(c -> c.index(indexName));
            return createIndexResponse.acknowledged();
        } catch (IOException e) {
            LOG.error("create index error");
        }
        return true;
    }

    /**
     * close client.
     */
    @Override
    public void close() {
        if (restClient != null && isStarted.get()) {
            try {
                transport.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
            try {
                restClient.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
            isStarted.set(false);
        }
    }

}
