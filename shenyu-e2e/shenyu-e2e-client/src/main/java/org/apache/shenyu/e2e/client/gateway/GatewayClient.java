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

import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.e2e.annotation.ShenYuGatewayClient;
import org.apache.shenyu.e2e.common.RequestLogConsumer;
import org.slf4j.MDC;

import java.util.Properties;
import java.util.function.Supplier;

import static io.restassured.RestAssured.given;

/**
 * A client to connect to ShenYu bootstrap(Gateway) server over HTTP.
 */
@ShenYuGatewayClient
@AllArgsConstructor
@Slf4j
public class GatewayClient {
    
    private final String scenarioId;
    
    @Getter
    private final String baseUrl;
    
    private final Properties properties;
    
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
    
}
