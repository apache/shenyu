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

package org.apache.shenyu.plugin.ai.prompt;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.AiPromptConfig;
import org.apache.shenyu.common.dto.convert.rule.AiPromptHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.view.AiRequestViewOperations;
import org.apache.shenyu.plugin.ai.prompt.handler.AiPromptPluginDataHandler;
import org.apache.shenyu.plugin.ai.protocol.AiProtocolDetector;
import org.apache.shenyu.plugin.ai.protocol.AiProtocols;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.exception.ResponsiveException;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.ServerWebExchangeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Ai Prompt Plugin.
 */
public class AiPromptPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AiPromptPlugin.class);

    /**
     * The input field of the OpenAI Responses protocol payloads.
     */
    private static final String INPUT = "input";

    private final List<HttpMessageReader<?>> messageReaders;
    
    public AiPromptPlugin(final List<HttpMessageReader<?>> messageReaders) {
        this.messageReaders = messageReaders;
    }
    
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        AiPromptConfig aiPromptConfig = Singleton.INST.get(AiPromptConfig.class);
        if (Objects.isNull(aiPromptConfig)) {
            aiPromptConfig = new AiPromptConfig();
        }
        
        AiPromptHandle aiPromptHandle = AiPromptPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        
        // Create final config with selector handle taking precedence
        if (Objects.nonNull(aiPromptHandle)) {
            aiPromptConfig = new AiPromptConfig();
            aiPromptConfig.setPreRole(aiPromptHandle.getPreRole());
            aiPromptConfig.setPrepend(aiPromptHandle.getPrepend());
            aiPromptConfig.setPostRole(aiPromptHandle.getPostRole());
            aiPromptConfig.setAppend(aiPromptHandle.getAppend());
        }
        
        final AiPromptConfig finalAiPromptConfig = aiPromptConfig;
        return ServerWebExchangeUtils.rewriteRequestBody(exchange,
                        messageReaders,
                        originalBody -> {
                            final String protocol = AiProtocolDetector.resolve(exchange, originalBody).orElse(null);
                            final String viewProtocol = Objects.isNull(protocol) ? AiProtocols.UNKNOWN : protocol;
                            // the first holder of the wire body publishes its bounded view once
                            AiRequestViewOperations.ensurePublished(exchange, originalBody, null, viewProtocol);
                            final String decoratedBody;
                            if (AiProtocols.OPENAI_CHAT.equals(protocol)) {
                                decoratedBody = decorateBody(originalBody, finalAiPromptConfig);
                            } else if (AiProtocols.OPENAI_RESPONSES.equals(protocol)) {
                                decoratedBody = decorateResponsesBody(originalBody, finalAiPromptConfig);
                            } else {
                                // unknown protocol: forward the body unchanged
                                decoratedBody = originalBody;
                            }
                            if (!Objects.equals(decoratedBody, originalBody)) {
                                // only a real mutation creates a new generation
                                AiRequestViewOperations.ensurePublished(exchange, decoratedBody,
                                        PluginEnum.AI_PROMPT.getName(), viewProtocol);
                            }
                            return Mono.just(decoratedBody);
                        }
                ).flatMap(chain::execute)
                .onErrorResume(error -> {
                    if (error instanceof ResponsiveException) {
                        return WebFluxResultUtils.failedResult((ResponsiveException) error);
                    }
                    return Mono.error(error);
                });
        
    }
    
    private String decorateBody(final String originalBody, final AiPromptConfig aiPromptConfig) {
        Map<String, Object> requestBodyMap = GsonUtils.getInstance().convertToMap(originalBody);
        Object rawMessages = requestBodyMap.get(Constants.MESSAGES);
        
        // If there is no message body, return directly
        if (Objects.isNull(rawMessages)) {
            return originalBody;
        }
        List messages = (List) rawMessages;
        if (CollectionUtils.isEmpty(messages)) {
            return originalBody;
        }
        
        List<Object> decoratedMessages = Lists.newArrayList();
        // If prepend in aiPromptConfig is not empty, add prepend to the front of message body
        if (Objects.nonNull(aiPromptConfig.getPrepend()) && Objects.nonNull(aiPromptConfig.getPreRole())) {
            // Assemble prepend content role
            Map<String, Object> prependMap = Maps.newHashMap();
            prependMap.put(Constants.CONTENT, aiPromptConfig.getPrepend());
            prependMap.put(Constants.ROLE, aiPromptConfig.getPreRole());
            decoratedMessages.add(prependMap);
        }
        decoratedMessages.addAll(messages);
        // If append in aiPromptConfig is not empty, add append to the end of message body
        if (Objects.nonNull(aiPromptConfig.getAppend()) && Objects.nonNull(aiPromptConfig.getPostRole())) {
            // Assemble append content role
            Map<String, Object> appendMap = Maps.newHashMap();
            appendMap.put(Constants.CONTENT, aiPromptConfig.getAppend());
            appendMap.put(Constants.ROLE, aiPromptConfig.getPostRole());
            decoratedMessages.add(appendMap);
        }
        requestBodyMap.put(Constants.MESSAGES, decoratedMessages);
        return GsonUtils.getInstance().toJson(requestBodyMap);
    }

    private String decorateResponsesBody(final String originalBody, final AiPromptConfig aiPromptConfig) {
        boolean injectPrepend = Objects.nonNull(aiPromptConfig.getPrepend()) && Objects.nonNull(aiPromptConfig.getPreRole());
        boolean injectAppend = Objects.nonNull(aiPromptConfig.getAppend()) && Objects.nonNull(aiPromptConfig.getPostRole());
        if (!injectPrepend && !injectAppend) {
            // nothing to inject: the wire body must stay byte identical
            return originalBody;
        }
        Map<String, Object> requestBodyMap = GsonUtils.getInstance().convertToMap(originalBody);
        if (Objects.isNull(requestBodyMap)) {
            // a non object body carries no input to decorate; forwarding it unchanged keeps the
            // failure to the protocol decoder instead of turning it into a server error
            return originalBody;
        }
        Object rawInput = requestBodyMap.get(INPUT);
        List<Object> inputItems = Lists.newArrayList();
        if (rawInput instanceof String) {
            // a string input is equivalent to a single user item and is converted to the items array
            Map<String, Object> userMap = Maps.newHashMap();
            userMap.put(Constants.CONTENT, rawInput);
            userMap.put(Constants.ROLE, "user");
            inputItems.add(userMap);
        } else if (rawInput instanceof List) {
            inputItems.addAll((List) rawInput);
        } else {
            return originalBody;
        }
        // If prepend in aiPromptConfig is not empty, add prepend to the front of input items
        if (injectPrepend) {
            // Assemble prepend content role
            Map<String, Object> prependMap = Maps.newHashMap();
            prependMap.put(Constants.CONTENT, aiPromptConfig.getPrepend());
            prependMap.put(Constants.ROLE, aiPromptConfig.getPreRole());
            inputItems.add(0, prependMap);
        }
        // If append in aiPromptConfig is not empty, add append to the end of input items
        if (injectAppend) {
            // Assemble append content role
            Map<String, Object> appendMap = Maps.newHashMap();
            appendMap.put(Constants.CONTENT, aiPromptConfig.getAppend());
            appendMap.put(Constants.ROLE, aiPromptConfig.getPostRole());
            inputItems.add(appendMap);
        }
        requestBodyMap.put(INPUT, inputItems);
        return GsonUtils.getInstance().toJson(requestBodyMap);
    }

    @Override
    public String named() {
        return PluginEnum.AI_PROMPT.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.AI_PROMPT.getCode();
    }
}
