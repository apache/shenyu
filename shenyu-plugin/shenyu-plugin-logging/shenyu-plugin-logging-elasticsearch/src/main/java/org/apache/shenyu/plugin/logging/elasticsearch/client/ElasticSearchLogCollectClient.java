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
import org.apache.shenyu.common.utils.LogUtils;
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
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * queue-based logging collector.
 */
public class ElasticSearchLogCollectClient extends AbstractLogConsumeClient<ElasticSearchLogCollectConfig.ElasticSearchLogConfig, ShenyuRequestLog> {

    private static final Logger LOG = LoggerFactory.getLogger(ElasticSearchLogCollectClient.class);
    
    private static final DateTimeFormatter DATE_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    private RestClient restClient;

    private ElasticsearchTransport transport;

    private ElasticsearchClient client;

    private String indexName = GenericLoggingConstant.INDEX;

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
        indexName = StringUtils.isNoneBlank(config.getIndexName()) ? config.getIndexName() : GenericLoggingConstant.INDEX;
        LogUtils.info(LOG, "init ElasticSearchLogCollectClient success");
        
        createOrUpdateIndexAlias(indexName);
    }

    /**
     * consume logs.
     * @param logs logs
     */
    @Override
    public void consume0(@NonNull final List<ShenyuRequestLog> logs) {
        String actualIndex = getActualIndexName();
        // Ensure the current day's index exists
        if (!existsIndex(actualIndex)) {
            createIndex(actualIndex);
            createOrUpdateIndexAlias(indexName);
        }
        
        List<BulkOperation> bulkOperations = new ArrayList<>();
        logs.forEach(log -> {
            try {
                bulkOperations.add(new BulkOperation.Builder().create(d -> d.document(log).index(indexName)).build());
            } catch (Exception e) {
                LogUtils.error(LOG, "add logs error: ", e);
            }
        });
        // Bulk storage
        try {
            client.bulk(e -> e.index(indexName).operations(bulkOperations));
        } catch (Exception e) {
            LogUtils.error(LOG, "elasticsearch store logs error: ", e);
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
            LogUtils.error(LOG, "fail to check the index exists, error:", e);
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
            LogUtils.error(LOG, "create index error:", e);
        }
    }
    
    /**
     * Get the actual index name for the current date.
     *
     * @return the actual index name with date suffix
     */
    private String getActualIndexName() {
        String date = LocalDate.now().format(DATE_FORMAT);
        return String.format("%s-%s", indexName, date);
    }
    
    /**
     * Create an index alias that points to all date-based indices.
     *
     * @param aliasName the alias name
     */
    private void createOrUpdateIndexAlias(final String aliasName) {
        try {
            String actualIndex = getActualIndexName();
            // Create the actual index if it doesn't exist
            if (!existsIndex(actualIndex)) {
                createIndex(actualIndex);
                LogUtils.info(LOG, "Created new date-based index: {}", actualIndex);
            }
            
            // Create or update the alias to point to the current index
            client.indices().putAlias(r -> r.index(actualIndex).name(aliasName));
            LogUtils.info(LOG, "Updated alias {} to point to index {}", aliasName, actualIndex);
        } catch (Exception e) {
            LogUtils.error(LOG, "Failed to create/update alias: ", e);
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
                LogUtils.error(LOG, "transport close has IOException : ", e);
            }
            try {
                restClient.close();
            } catch (IOException e) {
                LogUtils.error(LOG, "restClient close has IOException : ", e);
            }
        }
    }
}
