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

package org.apache.shenyu.e2e.client.gateway;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;
import org.apache.shenyu.e2e.annotation.ShenYuGatewayClient;
import org.apache.shenyu.e2e.common.RequestLogConsumer;
import org.apache.shenyu.e2e.model.data.MetaData;
import org.apache.shenyu.e2e.model.data.RuleCacheData;
import org.apache.shenyu.e2e.model.data.SelectorCacheData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.function.Supplier;

import static io.restassured.RestAssured.given;

/**
 * A client to connect to ShenYu bootstrap(Gateway) server over HTTP.
 */
@ShenYuGatewayClient
public class GatewayClient {

    private static final Logger log = LoggerFactory.getLogger(GatewayClient.class);
    
    private final String scenarioId;

    private final String baseUrl;
    
    private final Properties properties;

    private static final RestTemplate template = new RestTemplateBuilder().build();

    private static final ObjectMapper mapper = new ObjectMapper();

    public GatewayClient(String scenarioId, String baseUrl, Properties properties) {
        this.scenarioId = scenarioId;
        this.baseUrl = baseUrl;
        this.properties = properties;
    }

    /**
     * get baseUrl.
     *
     * @return baseUrl
     */
    public String getBaseUrl() {
        return baseUrl;
    }

    public Supplier<RequestSpecification> getHttpRequesterSupplier() {
        return () -> given().baseUri(getBaseUrl())
                .filter((req, resp, ctx) -> {
                    if (log.isDebugEnabled()) {
                        RequestLogConsumer.print(log, req);
                    } else {
                        log.info("Request: {} {}", req.getMethod(), req.getURI());
                    }
                    return ctx.next(req, resp);
                })
                .filter((req, resp, ctx) -> {
                    MDC.put("endpoint", req.getMethod() + " " + req.getURI());
                    
                    Response response = ctx.next(req, resp);
                    if (log.isDebugEnabled()) {
                        log.debug("request {} {}:\n{}", req.getMethod(), req.getURI(), response.asPrettyString());
                    }
                    return response;
                })
                .when();
    }

    public List<MetaData> getMetaDataCache() throws JsonProcessingException {
        ResponseEntity<List> response = template.exchange(baseUrl + "/actuator/metadata", HttpMethod.GET, null, List.class);
        List body = response.getBody();
        Map<String, MetaData> s = (Map<String, MetaData>) body.get(0);
        List<MetaData> metaDataList = new ArrayList<>();
        for (Map.Entry entry : s.entrySet()) {
            String json = mapper.writeValueAsString(entry.getValue());
            MetaData metaData = mapper.readValue(json, MetaData.class);
            metaDataList.add(metaData);
        }
        return metaDataList;
    }

    public List<SelectorCacheData> getSelectorCache() throws JsonProcessingException {
        ResponseEntity<List> response = template.exchange(baseUrl + "/actuator/selectorData", HttpMethod.GET, null, List.class);
        List body = response.getBody();
        Map<String, SelectorCacheData> s = (Map<String, SelectorCacheData>) body.get(0);
        List<SelectorCacheData> selectorDataList = new ArrayList<>();
        for (Map.Entry entry : s.entrySet()) {
            List list = (List) entry.getValue();
            String json = mapper.writeValueAsString(list.get(0));
            SelectorCacheData selectorData = mapper.readValue(json, SelectorCacheData.class);
            selectorDataList.add(selectorData);
        }
        return selectorDataList;
    }

    public List<RuleCacheData> getRuleCache() throws JsonProcessingException {
        ResponseEntity<List> response = template.exchange(baseUrl + "/actuator/ruleData", HttpMethod.GET, null, List.class);
        List body = response.getBody();
        Map<String, RuleCacheData> s = (Map<String, RuleCacheData>) body.get(0);
        List<RuleCacheData> ruleDataList = new ArrayList<>();
        for (Map.Entry entry : s.entrySet()) {
            List list = (List) entry.getValue();
            for (int i = 0; i < list.size(); i++) {
                String json = mapper.writeValueAsString(list.get(i));
                RuleCacheData ruleData = mapper.readValue(json, RuleCacheData.class);
                ruleDataList.add(ruleData);
            }
        }
        return ruleDataList;
    }
}
