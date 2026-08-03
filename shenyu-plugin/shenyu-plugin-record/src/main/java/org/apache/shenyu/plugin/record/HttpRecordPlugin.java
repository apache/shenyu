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

package org.apache.shenyu.plugin.record;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.HttpRecordHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.record.body.RecordServerHttpRequest;
import org.apache.shenyu.plugin.record.body.RecordServerHttpResponse;
import org.apache.shenyu.plugin.record.collector.HttpRecordCollector;
import org.apache.shenyu.plugin.record.entity.ShenyuHttpRequestRecord;
import org.apache.shenyu.plugin.record.handler.HttpRecordPluginDataHandler;
import org.apache.shenyu.plugin.record.utils.RecordUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Map;
import java.util.Objects;

/**
 * HttpRecordPlugin.
 *
 * <p>Records HTTP request and response data within a configured time window,
 * for later replay and debugging purposes.</p>
 */
public class HttpRecordPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(HttpRecordPlugin.class);

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {
        HttpRecordHandle httpRecordHandle = HttpRecordPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(httpRecordHandle)) {
            LOG.error("httpRecord rule handle is null, skip recording for rule: {}", rule.getId());
            return chain.execute(exchange);
        }
        long currentTime = System.currentTimeMillis();
        if (currentTime < httpRecordHandle.getStartTime() || currentTime > httpRecordHandle.getEndTime()) {
            return chain.execute(exchange);
        }
        ServerHttpRequest request = exchange.getRequest();
        Map<String, String> headers = RecordUtils.getHeaders(request.getHeaders());
        if (headers.containsKey(Constants.X_SHENYU_REPLAY)) {
            return chain.execute(exchange);
        }
        ShenyuHttpRequestRecord record = new ShenyuHttpRequestRecord();
        record.setTaskId(httpRecordHandle.getTaskId());
        record.setTraceId(UUIDUtils.getInstance().generateShortUuid());
        record.setMethod(request.getMethod().name());
        record.setQueryParams(request.getURI().getQuery());
        record.setRequestUri(request.getURI().getPath());
        record.setRequestHeaders(headers);

        RecordServerHttpRequest recordRequest =
                new RecordServerHttpRequest(request, record);
        RecordServerHttpResponse recordResponse =
                new RecordServerHttpResponse(exchange.getResponse(), record,
                        HttpRecordCollector.getInstance(), exchange);
        ServerWebExchange mutatedExchange = exchange.mutate()
                .request(recordRequest)
                .response(recordResponse)
                .build();

        return chain.execute(mutatedExchange);
    }

    @Override
    public String named() {
        return PluginEnum.HTTP_RECORD.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.HTTP_RECORD.getCode();
    }
}
