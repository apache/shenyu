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

package org.apache.shenyu.integrated.test.http.combination;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import co.elastic.clients.elasticsearch.core.SearchResponse;
import co.elastic.clients.json.jackson.JacksonJsonpMapper;
import co.elastic.clients.transport.ElasticsearchTransport;
import co.elastic.clients.transport.endpoints.BooleanResponse;
import co.elastic.clients.transport.rest_client.RestClientTransport;
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import org.apache.http.HttpHost;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.elasticsearch.client.RestClient;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class LoggingElasticSearchPluginTest extends AbstractPluginDataInit {

    private static final String LOGGING_ELASTIC_SEARCH_PATH = "/http/post/hi";

    private static RestClient restClient;

    private static ElasticsearchTransport transport;

    private static ElasticsearchClient client;

    private static final Logger LOG = LoggerFactory.getLogger(LoggingElasticSearchPluginTest.class);

    @BeforeAll
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.LOGGING_ELASTIC_SEARCH.getName(), "{\"host\":\"shenyu-elasticsearch\", \"port\": \"9200\"}");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.LOGGING_ELASTIC_SEARCH.getName(),
                "", buildSelectorConditionList(), buildRuleLocalDataList());
        assertThat(selectorAndRulesResult, is("success"));
        restClient = RestClient
                .builder(new HttpHost("localhost", 9200))
                .build();
        transport = new RestClientTransport(restClient, new JacksonJsonpMapper());
        client = new ElasticsearchClient(transport);
        OkHttpClient client = new OkHttpClient();
        MediaType mediaType = MediaType.parse("application/json");
        RequestBody body = RequestBody.create(mediaType, "{\"number_of_replicas\":0}");
        Request request = new Request.Builder()
                .url("http://localhost:9200/_settings")
                .put(body)
                .addHeader("Content-Type", "application/json")
                .addHeader("cache-control", "no-cache")
                .build();
        Response response = client.newCall(request).execute();
        LOG.info("response {},", response.body().string());
        Request balanceRequest = new Request.Builder()
                .url("http://localhost:9200/_cluster/reroute?retry_failed=true")
                .post(null)
                .addHeader("cache-control", "no-cache")
                .build();
        Response balanceResponse = client.newCall(balanceRequest).execute();
        LOG.info("balanceResponse {},", balanceResponse.body().string());
    }

    @Test
    public void testElasticSearchPlugin() throws Exception {
        String result = HttpHelper.INSTANCE.postGateway(LOGGING_ELASTIC_SEARCH_PATH, String.class);
        assertNotNull(result);
        Thread.sleep(3000);
        BooleanResponse existsResponse = client.indices().exists(c -> c.index("shenyu-access-logging"));
        assertThat(String.valueOf(existsResponse.value()), is("true"));
        SearchResponse<ShenyuRequestLog> searchResponse = client.search(a -> a.index("shenyu-access-logging"), ShenyuRequestLog.class);
        ShenyuRequestLog shenyuRequestLog = searchResponse.hits().hits()
                .get(searchResponse.hits().hits().size() - 1).source();
        assertThat(Objects.requireNonNull(shenyuRequestLog).getMethod(), is("/http/post/hi"));
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.STARTS_WITH.getAlias());
        conditionData.setParamValue("/http/post/hi");
        return Collections.singletonList(conditionData);
    }

    private static List<RuleLocalData> buildRuleLocalDataList() {
        final RuleLocalData ruleLocalData = new RuleLocalData();
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue("/http/post/hi");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        return Collections.singletonList(ruleLocalData);
    }

    @AfterAll
    public static void clean() throws IOException {
        restClient.close();
        transport.close();
        cleanPluginData(PluginEnum.LOGGING_ELASTIC_SEARCH.getName());
    }
}
