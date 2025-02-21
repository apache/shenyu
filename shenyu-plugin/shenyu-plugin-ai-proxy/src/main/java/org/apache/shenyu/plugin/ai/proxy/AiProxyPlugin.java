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

package org.apache.shenyu.plugin.ai.proxy;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.AiProxyConfig;
import org.apache.shenyu.common.enums.AiModelProviderEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.proxy.strategy.AiModel;
import org.apache.shenyu.plugin.ai.proxy.strategy.AiModelFactory;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.Objects;

/**
 * this is ai proxy plugin.
 */
public class AiProxyPlugin extends AbstractShenyuPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
            final SelectorData selector, final RuleData rule) {
        AiProxyConfig aiProxyConfig = Singleton.INST.get(AiProxyConfig.class);
        if (Objects.isNull(aiProxyConfig)) {
            return chain.execute(exchange);
        }

        return DataBufferUtils.join(exchange.getRequest().getBody())
                .flatMap(dataBuffer -> {
                    byte[] bytes = new byte[dataBuffer.readableByteCount()];
                    dataBuffer.read(bytes);
                    DataBufferUtils.release(dataBuffer);
                    String requestBody = new String(bytes, StandardCharsets.UTF_8);
                    
                    // choose the model by provider
                    String provider = aiProxyConfig.getProvider();
                    AiModelProviderEnum providerEnum = AiModelProviderEnum.getByName(provider);
                    assert Objects.nonNull(providerEnum);
                    AiModel aiModel = AiModelFactory.createAiModel(providerEnum);
                    assert Objects.nonNull(aiModel);
                    return aiModel.invoke(aiProxyConfig, exchange, requestBody);
                });
    }


    @Override
    public int getOrder() {
        return PluginEnum.AI_PROXY.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.AI_PROXY.getName();
    }
}
