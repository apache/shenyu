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

package org.apache.shenyu.plugin.param.mapping;

import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.ParamMappingHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.param.mapping.handler.ParamMappingPluginDataHandler;
import org.apache.shenyu.plugin.param.mapping.strategy.Operator;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Map;
import java.util.Objects;

/**
 * ParamMappingPlugin.
 */
@Slf4j
public class ParamMappingPlugin extends AbstractShenyuPlugin {

    private final Map<String, Operator> operatorMap;

    public ParamMappingPlugin(final Map<String, Operator> operatorMap) {
        this.operatorMap = operatorMap;
    }

    @Override
    public Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        ParamMappingHandle paramMappingHandle = ParamMappingPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(paramMappingHandle)) {
            log.error("param mapping rule configuration is null :{}", rule.getId());
            return chain.execute(exchange);
        }
        HttpHeaders headers = exchange.getRequest().getHeaders();
        MediaType contentType = headers.getContentType();
        return match(contentType).apply(exchange, chain, paramMappingHandle);
    }

    @Override
    public int getOrder() {
        return PluginEnum.PARAM_MAPPING.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.PARAM_MAPPING.getName();
    }

    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        return false;
    }

    /**
     * OperatorFactory match.
     *
     * @param mediaType mediaType
     * @return operator
     */
    private Operator match(final MediaType mediaType) {
        if (MediaType.APPLICATION_JSON.isCompatibleWith(mediaType)) {
            return operatorMap.get(MediaType.APPLICATION_JSON.toString());
        } else if (MediaType.APPLICATION_FORM_URLENCODED.isCompatibleWith(mediaType)) {
            return operatorMap.get(MediaType.APPLICATION_FORM_URLENCODED.toString());
        } else {
            return operatorMap.get(Constants.DEFAULT);
        }
    }
}
