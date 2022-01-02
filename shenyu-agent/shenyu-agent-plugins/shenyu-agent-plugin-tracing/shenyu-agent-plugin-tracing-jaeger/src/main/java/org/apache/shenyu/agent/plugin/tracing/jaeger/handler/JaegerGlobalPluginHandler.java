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

package org.apache.shenyu.agent.plugin.tracing.jaeger.handler;

import io.opentracing.Span;
import io.opentracing.tag.Tags;
import org.apache.shenyu.agent.api.entity.MethodResult;
import org.apache.shenyu.agent.api.entity.TargetObject;
import org.apache.shenyu.agent.api.handler.InstanceMethodHandler;
import org.apache.shenyu.agent.plugin.tracing.jaeger.constant.JaegerConstants;
import org.apache.shenyu.agent.plugin.tracing.jaeger.span.JaegerSpanManager;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * The type Jaeger global plugin handler.
 */
public final class JaegerGlobalPluginHandler implements InstanceMethodHandler {

    @Override
    public void before(final TargetObject target, final Method method, final Object[] args, final MethodResult result) {
        final ServerWebExchange exchange = (ServerWebExchange) args[0];
        final JaegerSpanManager jaegerSpanManager = (JaegerSpanManager) exchange.getAttributes()
                .getOrDefault(JaegerConstants.ROOT_SPAN, new JaegerSpanManager());

        Map<String, String> tagMap = new HashMap<>(4);
        tagMap.put(Tags.COMPONENT.getKey(), JaegerConstants.NAME);
        tagMap.put(Tags.HTTP_URL.getKey(), exchange.getRequest().getURI().toString());
        Optional.ofNullable(exchange.getRequest().getMethod())
                        .ifPresent(v -> tagMap.put(Tags.HTTP_STATUS.getKey(), v.toString()));

        Span span = jaegerSpanManager.add(JaegerConstants.ROOT_SPAN, tagMap);
        exchange.getAttributes().put(JaegerConstants.RESPONSE_SPAN, jaegerSpanManager);
        target.setContext(span);
    }

    @Override
    public Object after(final TargetObject target, final Method method, final Object[] args, final MethodResult methodResult, final Object result) {
        Span span = (Span) target.getContext();
        ServerWebExchange exchange = (ServerWebExchange) args[0];
        JaegerSpanManager manager = (JaegerSpanManager) exchange.getAttributes().get(JaegerConstants.ROOT_SPAN);

        if (result instanceof Mono) {
            return ((Mono) result).doFinally(s -> {
                manager.finish(span, exchange);
            });
        }

        manager.finish(span, exchange);
        return result;
    }

    @Override
    public void onThrowing(final TargetObject target, final Method method, final Object[] args, final Throwable throwable) {
        Span span = (Span) target.getContext();

        ServerWebExchange exchange = (ServerWebExchange) args[0];
        JaegerSpanManager manager = (JaegerSpanManager) exchange.getAttributes().get(JaegerConstants.ROOT_SPAN);

        manager.error(span, exchange, throwable);
    }
}
