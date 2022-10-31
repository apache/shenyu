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
import co.elastic.clients.elasticsearch.core.bulk.BulkOperation;
import co.elastic.clients.json.jackson.JacksonJsonpMapper;
import co.elastic.clients.transport.ElasticsearchTransport;
import co.elastic.clients.transport.endpoints.BooleanResponse;
import co.elastic.clients.transport.rest_client.RestClientTransport;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpHost;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.elasticsearch.config.ElasticSearchLogCollectConfig;
import org.elasticsearch.client.RestClient;
import org.elasticsearch.client.RestClientBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * queue-based logging collector.
 */
public class ElasticSearchLogCollectClient extends AbstractLogConsumeClient<ElasticSearchLogCollectConfig.ElasticSearchLogConfig, ShenyuRequestLog> {

    private static final Logger LOG = LoggerFactory.getLogger(ElasticSearchLogCollectClient.class);

    private RestClient restClient;

    private ElasticsearchTransport transport;

    private ElasticsearchClient client;

    /**
     * init elasticsearch client.
     *
     * @param config elasticsearch client config
     */
    @Override
    public void initClient0(@NonNull final ElasticSearchLogCollectConfig.ElasticSearchLogConfig config) {
        RestClientBuilder builder = RestClient
                .builder(new HttpHost(config.getHost(), Integer.parseInt(config.getPort())));
        
        // authentication and config auth cathe.
        if (StringUtils.isNoneBlank(config.getUsername()) && StringUtils.isNoneBlank(config.getPassword())) {
            final CredentialsProvider credentialsProvider = new BasicCredentialsProvider();
            credentialsProvider.setCredentials(AuthScope.ANY,
                    new UsernamePasswordCredentials(config.getUsername(), config.getPassword()));
            builder.setHttpClientConfigCallback(asyncClientBuilder -> {
                if (Boolean.FALSE.equals(config.getAuthCache())) {
                    asyncClientBuilder.disableAuthCaching();
                }
                return asyncClientBuilder.setDefaultCredentialsProvider(credentialsProvider);
            });
        }
        
        restClient = builder.build();
        transport = new RestClientTransport(restClient, new JacksonJsonpMapper());
        client = new ElasticsearchClient(transport);
        LOG.info("init ElasticSearchLogCollectClient success");
        // Determine whether the index exists, and create it if it does not exist
        if (!existsIndex(GenericLoggingConstant.INDEX)) {
            createIndex(GenericLoggingConstant.INDEX);
            LOG.info("create index success");
        }
    }

    @Override
    public void consume0(@NonNull final List<ShenyuRequestLog> logs) {
        List<BulkOperation> bulkOperations = new ArrayList<>();
        logs.forEach(log -> {
            try {
                bulkOperations.add(new BulkOperation.Builder().create(d -> d.document(log).index(GenericLoggingConstant.INDEX)).build());
            } catch (Exception e) {
                LOG.error("add logs error", e);
            }
        });
        // Bulk storage
        try {
            client.bulk(e -> e.index(GenericLoggingConstant.INDEX).operations(bulkOperations));
        } catch (Exception e) {
            LOG.error("elasticsearch store logs error", e);
        }
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
     * create elasticsearch index.
     *
     * @param indexName index name
     */
    public void createIndex(final String indexName) {
        try {
            client.indices().create(c -> c.index(indexName));
        } catch (IOException e) {
            LOG.error("create index error");
        }
    }

    /**
     * close client.
     */
    @Override
    public void close0() {
        if (Objects.nonNull(restClient)) {
            try {
                transport.close();
            } catch (IOException e) {
                LOG.error("transport close has IOException : ", e);
            }
            try {
                restClient.close();
            } catch (IOException e) {
                LOG.error("restClient close has IOException : ", e);
            }
        }
    }
}
