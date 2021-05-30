package org.apache.shenyu.plugin.response;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.ParamResponseRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.response.cache.ParamResponseRuleHandleCache;
import org.apache.shenyu.plugin.response.handler.ParamResponsePluginDataHandler;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.util.CollectionUtils;
import org.springframework.web.reactive.function.BodyExtractors;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;
import java.util.Objects;

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


/**
 * ParamResponse plugin.
 */
@Slf4j
public class ParamResponsePlugin extends AbstractShenyuPlugin {

    private final static String START_TIME = "START_TIME";

    public ParamResponsePlugin() {

    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        if (Objects.isNull(rule)) {
            return Mono.empty();
        }
        final ShenyuContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        assert soulContext != null;
        exchange.getAttributes().put(START_TIME, System.currentTimeMillis());
        log.info("test response plugin start");
        final ParamResponseRuleHandle paramResponseRuleHandle = ParamResponseRuleHandleCache.getInstance().obtainHandle(ParamResponsePluginDataHandler.getResourceName(rule));

        if(Objects.nonNull(paramResponseRuleHandle)){
            ServerHttpResponse response = exchange.getResponse();
            HttpHeaders httpHeaders = response.getHeaders();
            if(Objects.nonNull(paramResponseRuleHandle.getAddHeaderMap()) && MapUtils.isNotEmpty(paramResponseRuleHandle.getAddHeaderMap())){
                Map<String,String> addHeaderMap = paramResponseRuleHandle.getAddHeaderMap();
                addHeaderMap.entrySet().stream().forEach(a -> httpHeaders.add(a.getKey(),a.getValue()));
            }

            if(Objects.nonNull(paramResponseRuleHandle.getSetHeaderMap()) && MapUtils.isNotEmpty(paramResponseRuleHandle.getSetHeaderMap())){
                Map<String,String> setHeaderMap = paramResponseRuleHandle.getSetHeaderMap();
                setHeaderMap.entrySet().stream().forEach(a -> httpHeaders.set(a.getKey(),a.getValue()));
            }

            if(Objects.nonNull(paramResponseRuleHandle.getReplaceHeaderMap()) && MapUtils.isNotEmpty(paramResponseRuleHandle.getReplaceHeaderMap())){
                Map<String,String> replaceHeaderMap = paramResponseRuleHandle.getReplaceHeaderMap();
                replaceHeaderMap.entrySet().stream().forEach(a -> {
                    httpHeaders.addAll(a.getValue(),httpHeaders.get(a.getKey()));
                    httpHeaders.remove(a.getKey());
                });
            }

            if(Objects.nonNull(paramResponseRuleHandle.getRemoveHeaderList()) && !CollectionUtils.isEmpty(paramResponseRuleHandle.getRemoveHeaderList())){
                List<String> removeHeaderList = paramResponseRuleHandle.getRemoveHeaderList();
                removeHeaderList.stream().forEach(a -> httpHeaders.remove(a));
            }

            ClientResponse clientResponse = exchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR);
            if(paramResponseRuleHandle.getStatusCode() > 0){

                response.setStatusCode(HttpStatus.valueOf(paramResponseRuleHandle.getStatusCode()));
            }else{
                response.setStatusCode(clientResponse.statusCode());
            }

        }

        return chain.execute(exchange)
                .then(Mono.defer(() -> {
                    Long startTime = exchange.getAttribute(START_TIME);
                    log.info("ResponsePlugin end. Total execution time: {} ms", System.currentTimeMillis() - startTime);
                    ServerHttpResponse response = exchange.getResponse();
                    ClientResponse clientResponse = exchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR);
                    return response.writeWith(clientResponse.body(BodyExtractors.toDataBuffers()));
                }));
    }


    @Override
    public int getOrder() {
        return PluginEnum.PARAM_RESPONSE.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.PARAM_RESPONSE.getName();
    }

}
