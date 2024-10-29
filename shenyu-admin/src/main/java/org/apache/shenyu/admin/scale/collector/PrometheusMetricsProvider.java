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

import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.admin.scale.collector.provider.MetricData;
import org.apache.shenyu.admin.scale.collector.provider.MetricsProvider;
import org.apache.shenyu.admin.config.properties.DeploymentProperties;
import org.apache.shenyu.admin.scale.config.PrometheusProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.io.IOException;

@Component
public class PrometheusMetricsProvider implements MetricsProvider {

    private static final Logger LOG = LoggerFactory.getLogger(PrometheusMetricsProvider.class);

    private final PrometheusProperties prometheusProperties;

    private final DeploymentProperties deploymentProperties;

    private final OkHttpClient httpClient;

    private final ObjectMapper objectMapper;

    public PrometheusMetricsProvider(final PrometheusProperties prometheusProperties,
                                     final DeploymentProperties deploymentProperties) {
        this.prometheusProperties = prometheusProperties;
        this.deploymentProperties = deploymentProperties;
        this.httpClient = new OkHttpClient();
        this.objectMapper = new ObjectMapper();
    }

    @Override
    public MetricData getMetricData(final String metricName) {

        String queryTemplate = prometheusProperties.getQueries().get(metricName);
        if (queryTemplate == null) {
            LOG.error("No query template found for metric: {}", metricName);
            return null;
        }

        String query = String.format(queryTemplate, deploymentProperties.getNamespace(), deploymentProperties.getName());
        String queryUrl = String.format("%s/api/v1/query?query=%s", prometheusProperties.getUrl(), query);

        Request request = new Request.Builder()
                .url(queryUrl)
                .get()
                .build();

        try (Response response = httpClient.newCall(request).execute()) {

            if (!response.isSuccessful()) {
                LOG.error("Failed to fetch metrics from Prometheus: {}", response.message());
                return null;
            }

            if (response.body() == null) {
                LOG.error("Response body is null for metric: {}", metricName);
                return null;
            }

            JsonNode jsonNode = objectMapper.readTree(response.body().string());
            return parseMetricData(metricName, jsonNode);

        } catch (IOException e) {
            LOG.error("Failed to get metric data. cause: {} ", e.getMessage());
            return null;
        }
    }

    /**
     * parseMetricData.
     *
     * @param metricName metricName
     * @param jsonNode jsonNode
     * @return MetricData
     */
    private MetricData parseMetricData(final String metricName, final JsonNode jsonNode) {
        JsonNode statusNode = jsonNode.get("status");
        if (statusNode == null || !"success".equals(statusNode.asText())) {
            LOG.error("Failed to fetch metric: {} ", metricName);
            return null;
        }

        JsonNode dataNode = jsonNode.get("data").get("result");
        if (dataNode.isEmpty()) {
            LOG.error("No data found for metric: {} ", metricName);
            return null;
        }

        JsonNode valueNode = dataNode.get(0).get("value");
        if (valueNode == null || valueNode.size() < 2) {
            LOG.error("Invalid metric data format for: {} ", metricName);
            return null;
        }

        try {
            double value = Double.parseDouble(valueNode.get(1).asText());
            return new MetricData(metricName, value);
        } catch (NumberFormatException e) {
            LOG.error("Failed to parse metric value for: {} ", metricName);
            return null;
        }
    }
}
