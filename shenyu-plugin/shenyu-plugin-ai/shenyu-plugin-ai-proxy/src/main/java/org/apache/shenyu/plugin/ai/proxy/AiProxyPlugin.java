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

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.AiModelProviderEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.common.strategy.AiModel;
import org.apache.shenyu.plugin.ai.common.strategy.AiModelFactory;
import org.apache.shenyu.plugin.ai.proxy.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * this is ai proxy plugin.
 */
public class AiProxyPlugin extends AbstractShenyuPlugin {
    
    private static final Logger LOG = LoggerFactory.getLogger(AiProxyPlugin.class);
    
    private final List<HttpMessageReader<?>> messageReaders;
    
    public AiProxyPlugin(final List<HttpMessageReader<?>> messageReaders) {
        this.messageReaders = messageReaders;
    }
    
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {
        AiCommonConfig aiCommonConfig = Singleton.INST.get(AiCommonConfig.class);
        if (Objects.isNull(aiCommonConfig)) {
            aiCommonConfig = new AiCommonConfig();
        }
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        Objects.requireNonNull(shenyuContext);
        
        // Get selector handle from cache
        AiProxyHandle selectorHandle = AiProxyPluginHandler.SELECTOR_CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(selector.getId(), Constants.DEFAULT_RULE));
        
        // Create final config with selector handle taking precedence
        if (Objects.nonNull(selectorHandle)) {
            AiCommonConfig tmp = new AiCommonConfig();
            tmp.setProvider(Optional.ofNullable(selectorHandle.getProvider()).orElse(aiCommonConfig.getProvider()));
            tmp.setBaseUrl(Optional.ofNullable(selectorHandle.getBaseUrl()).orElse(aiCommonConfig.getBaseUrl()));
            tmp.setApiKey(Optional.ofNullable(selectorHandle.getApiKey()).orElse(aiCommonConfig.getApiKey()));
            tmp.setModel(Optional.ofNullable(selectorHandle.getModel()).orElse(aiCommonConfig.getModel()));
            tmp.setTemperature(Optional.ofNullable(selectorHandle.getTemperature()).orElse(aiCommonConfig.getTemperature()));
            tmp.setMaxTokens(Optional.ofNullable(selectorHandle.getMaxTokens()).orElse(aiCommonConfig.getMaxTokens()));
            tmp.setStream(Optional.ofNullable(selectorHandle.getStream()).orElse(aiCommonConfig.getStream()));
            aiCommonConfig = tmp;
        }
        
        if (Objects.isNull(aiCommonConfig.getBaseUrl())) {
            LOG.error("AI proxy plugin: baseUrl is null");
            return chain.execute(exchange);
        }
        
        shenyuContext.setRpcType(RpcTypeEnum.AI.getName());
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        
        // set domain
        exchange.getAttributes().put(Constants.HTTP_DOMAIN, aiCommonConfig.getBaseUrl());
        // set the http timeout
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 60 * 3000L);
        exchange.getAttributes().put(Constants.HTTP_RETRY, 0);
        
        String provider = aiCommonConfig.getProvider();
        AiModelProviderEnum providerEnum = AiModelProviderEnum.getByName(provider);
        if (Objects.isNull(providerEnum)) {
            return Mono.error(new IllegalArgumentException("Invalid AI model provider"));
        }
        // Create AI model instance
        AiModel aiModel = AiModelFactory.createAiModel(providerEnum);
        if (Objects.isNull(aiModel)) {
            return Mono.error(new IllegalStateException("Failed to create AI model"));
        }
        
        exchange.getAttributes().put(Constants.AI_MODEL, aiModel);
        
        return aiModel.invoke(aiCommonConfig, exchange, chain, messageReaders);
        
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
