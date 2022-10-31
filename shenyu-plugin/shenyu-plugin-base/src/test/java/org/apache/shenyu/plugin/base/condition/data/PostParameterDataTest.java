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

package org.apache.shenyu.plugin.base.condition.data;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.HttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for {@link PostParameterData}.
 */
public final class PostParameterDataTest {

    private ServerWebExchange exchange;

    private PostParameterData postParameterData;

    @BeforeEach
    public void setUp() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/uri/path")
                .remoteAddress(new InetSocketAddress("localhost", 8080))
                .build());
        ShenyuContext context = new ShenyuContext();
        context.setRpcType(RpcTypeEnum.HTTP.getName());
        context.setHttpMethod(HttpMethodEnum.POST.getName());
        this.exchange.getAttributes().put(Constants.CONTEXT, context);
        this.postParameterData = new PostParameterData();
    }

    @Test
    public void testBuilder() {
        assertEquals("post", this.postParameterData.builder("httpMethod", this.exchange));
        assertEquals("http", this.postParameterData.builder("rpcType", this.exchange));
    }
}
