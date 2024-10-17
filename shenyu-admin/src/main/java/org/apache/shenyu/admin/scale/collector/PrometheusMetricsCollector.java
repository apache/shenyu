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

package org.apache.shenyu.admin.scale.collector;

import com.fasterxml.jackson.core.JsonProcessingException;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import org.apache.shenyu.admin.scale.collector.provider.MetricData;

import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;

public class PrometheusMetricsCollector {

    private final String prometheusUrl;

    public PrometheusMetricsCollector(final String prometheusUrl) {
        this.prometheusUrl = prometheusUrl;
    }

    /**
     * query metrics.
     * @param query query
     * @return List
     */
    public List<MetricData> queryMetrics(final String query) throws Exception {
        String response = executeQuery(query);
        return parseResponse(response);
    }

    /**
     * execute query.
     * @param query query
     * @return String
     */
    private String executeQuery(final String query) throws Exception {
        OkHttpClient client = new OkHttpClient();

        String queryUrl = String.format("%s/api/v1/query?query=%s", prometheusUrl, URLEncoder.encode(query, StandardCharsets.UTF_8));

        Request request = new Request.Builder()
                .url(queryUrl)
                .build();

        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new IOException("Unexpected code " + response);
            }

            assert response.body() != null;
            return response.body().string();
        }
    }

    /**
     * parse response.
     * @param response response
     * @return List
     */
    private List<MetricData> parseResponse(final String response) throws JsonProcessingException {
        ObjectMapper objectMapper = new ObjectMapper();
        JsonNode rootNode = objectMapper.readTree(response);

        JsonNode dataNode = rootNode.path("data").path("result");

        List<MetricData> metrics = new ArrayList<>();
        for (JsonNode resultNode : dataNode) {
            String metricName = resultNode.path("metric").path("__name__").asText();
            double value = resultNode.path("value").get(1).asDouble();

            MetricData metricData = new MetricData(metricName, value);
            metrics.add(metricData);
        }

        return metrics;
    }
}
