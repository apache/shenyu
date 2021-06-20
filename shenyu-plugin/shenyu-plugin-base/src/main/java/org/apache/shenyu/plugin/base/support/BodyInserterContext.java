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

package org.apache.shenyu.plugin.base.support;

import org.springframework.http.codec.HttpMessageWriter;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.reactive.function.BodyInserter;
import org.springframework.web.reactive.function.client.ExchangeStrategies;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * The type Body inserter context.
 * 
 * @see <a href="https://github.com/spring-cloud/spring-cloud-gateway/blob/master/spring-cloud-gateway-server/src/main/java/org/springframework/cloud/gateway/support/BodyInserterContext.java">BodyInserterContext</a>
 */
public class BodyInserterContext implements BodyInserter.Context {

    private final ExchangeStrategies exchangeStrategies;

    /**
     * Instantiates a new Body inserter context.
     */
    public BodyInserterContext() {
        this.exchangeStrategies = ExchangeStrategies.withDefaults();
    }

    @Override
    public List<HttpMessageWriter<?>> messageWriters() {
        return exchangeStrategies.messageWriters();
    }

    @Override
    public Optional<ServerHttpRequest> serverRequest() {
        return Optional.empty();
    }

    @Override
    public Map<String, Object> hints() {
        return Collections.emptyMap();
    }
}
