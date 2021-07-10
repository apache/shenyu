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

package org.apache.shenyu.plugin.param.mapping.strategy;

import org.apache.shenyu.common.dto.convert.rule.impl.ParamMappingHandle;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link JsonOperator}.
 */
@RunWith(MockitoJUnitRunner.class)
public class JsonOperatorTest {

    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private JsonOperator jsonOperator;

    private ParamMappingHandle paramMappingHandle;

    @Before
    public void setUp() {
        Set<String> remove = new HashSet<>();
        remove.add("$.age");
        ParamMappingHandle.ParamMapInfo add = new ParamMappingHandle.ParamMapInfo();
        add.setPath("$");
        add.setKey("webName");
        add.setValue("SOUL");
        ParamMappingHandle.ParamMapInfo replace = new ParamMappingHandle.ParamMapInfo();
        replace.setPath("$");
        replace.setKey("name");
        replace.setValue("realName");
        this.paramMappingHandle = new ParamMappingHandle();
        this.paramMappingHandle.setRemoveParameterKeys(remove);
        this.paramMappingHandle.setAddParameterKeys(Arrays.asList(add));
        this.paramMappingHandle.setReplaceParameterKeys(Arrays.asList(replace));
        this.jsonOperator = new JsonOperator();
        final String body = "{\"name\":\"shenyu\",\"age\":\"18\"}";
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.method(HttpMethod.POST, "localhost")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE).body(body));
    }

    @Test
    public void testApply() {
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        StepVerifier.create(jsonOperator.apply(this.exchange, this.chain, paramMappingHandle)).expectSubscription().verifyComplete();
    }
}

