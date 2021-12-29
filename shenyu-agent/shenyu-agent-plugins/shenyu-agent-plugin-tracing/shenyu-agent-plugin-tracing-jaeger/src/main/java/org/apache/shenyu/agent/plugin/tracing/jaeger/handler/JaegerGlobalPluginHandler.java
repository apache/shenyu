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
import io.opentracing.util.GlobalTracer;
import org.apache.shenyu.agent.api.entity.MethodResult;
import org.apache.shenyu.agent.api.entity.TargetObject;
import org.apache.shenyu.agent.api.handler.InstanceMethodHandler;
import org.apache.shenyu.agent.plugin.tracing.jaeger.constant.JaegerConstants;
import org.apache.shenyu.agent.plugin.tracing.jaeger.span.JaegerErrorSpan;

import java.lang.reflect.Method;

/**
 * The type Jaeger global plugin handler.
 */
public final class JaegerGlobalPluginHandler implements InstanceMethodHandler {
    
    @Override
    public void before(final TargetObject target, final Method method, final Object[] args, final MethodResult result) {
        Span scope = GlobalTracer.get().buildSpan(JaegerConstants.ROOT_SPAN)
                .withTag(Tags.COMPONENT.getKey(), JaegerConstants.NAME).start();
        target.setContext(scope);
    }
    
    @Override
    public void after(final TargetObject target, final Method method, final Object[] args, final MethodResult result) {
        ((Span) target.getContext()).finish();
    }
    
    @Override
    public void onThrowing(final TargetObject target, final Method method, final Object[] args, final Throwable throwable) {
        JaegerErrorSpan.setError(GlobalTracer.get().activeSpan(), throwable);
    }
}
