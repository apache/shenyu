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

package org.dromara.soul.plugin.apache.dubbo.param;

import org.apache.dubbo.common.utils.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.junit.Before;
import org.junit.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.test.StepVerifier;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;

/**
 * The Test Case For BodyParamPlugin.
 *
 * @author nuo-promise
 **/
public final class BodyParamPluginTest {

    private BodyParamPlugin bodyParamPlugin;

    @Before
    public void setUp() {
        bodyParamPlugin = new BodyParamPlugin();
    }

    @Test(expected = NullPointerException.class)
    public void testExecute() {
        execute(null, null);
    }

    @Test(expected = NullPointerException.class)
    public void testExecuteNoContentTypeName() {
        execute(RpcTypeEnum.DUBBO.getName(), null);
    }

    @Test
    public void testExecuteHaveJsonContentName() {
        execute(RpcTypeEnum.DUBBO.getName(), "application/json");
    }

    @Test
    public void testExecuteHaveWwwContentName() {
        execute(RpcTypeEnum.DUBBO.getName(), "application/x-www-form-urlencoded");
    }

    private void execute(final String rpcTypeName, final String contentTypeName) {
        ServerWebExchange exchange = MockServerWebExchange
                .from(MockServerHttpRequest.get("http://localhost:8888/test").build());
        SoulPluginChain chain = mock(SoulPluginChain.class);
        SoulContext soulContext = new SoulContext();

        if (StringUtils.isNotEmpty(rpcTypeName)) {
            soulContext.setRpcType(rpcTypeName);
            if (StringUtils.isNotEmpty(contentTypeName)) {
                exchange = MockServerWebExchange
                        .from(MockServerHttpRequest.get("http://localhost:8888/test").header("Content-Type", contentTypeName).build());
            }
            exchange.getAttributes().put(Constants.CONTEXT, soulContext);
        }

        StepVerifier.create(bodyParamPlugin.execute(exchange, chain)).expectError(NullPointerException.class).verifyThenAssertThat();
    }

    @Test
    public void getOrder() {
        assertThat(bodyParamPlugin.getOrder(), is(PluginEnum.DUBBO.getCode() - 1));
    }

    @Test
    public void named() {
        assertThat(bodyParamPlugin.named(), is("apache-dubbo-body-param"));
    }
}
