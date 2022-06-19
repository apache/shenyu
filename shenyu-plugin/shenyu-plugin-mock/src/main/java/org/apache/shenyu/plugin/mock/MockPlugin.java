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

package org.apache.shenyu.plugin.mock;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.MockHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.mock.handler.MockPluginHandler;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * MockPlugin.
 */
public class MockPlugin extends AbstractShenyuPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange,
                                   final ShenyuPluginChain chain,
                                   final SelectorData selector,
                                   final RuleData rule) {
        MockHandle mockHandle = MockPluginHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        return WebFluxResultUtils.resultCustomStatusCode(exchange, mockHandle.getResponseContent(), HttpStatus.valueOf(mockHandle.getHttpStatusCode()));
    }

    @Override
    public int getOrder() {
        return PluginEnum.MOCK.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.MOCK.getName();
    }

}
