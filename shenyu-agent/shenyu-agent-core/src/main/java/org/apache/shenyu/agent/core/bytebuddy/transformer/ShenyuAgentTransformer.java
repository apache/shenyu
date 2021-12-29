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

package org.apache.shenyu.agent.core.bytebuddy.transformer;

import net.bytebuddy.agent.builder.AgentBuilder.Transformer;
import net.bytebuddy.description.method.MethodDescription;
import net.bytebuddy.description.method.MethodDescription.InDefinedShape;
import net.bytebuddy.description.type.TypeDescription;
import net.bytebuddy.dynamic.DynamicType.Builder;
import net.bytebuddy.implementation.FieldAccessor;
import net.bytebuddy.implementation.MethodDelegation;
import net.bytebuddy.implementation.SuperMethodCall;
import net.bytebuddy.jar.asm.Opcodes;
import net.bytebuddy.matcher.ElementMatchers;
import net.bytebuddy.utility.JavaModule;
import org.apache.shenyu.agent.api.entity.TargetObject;
import org.apache.shenyu.agent.api.handler.ConstructorHandler;
import org.apache.shenyu.agent.api.handler.InstanceMethodHandler;
import org.apache.shenyu.agent.api.handler.StaticMethodHandler;
import org.apache.shenyu.agent.api.point.ConstructorPointCut;
import org.apache.shenyu.agent.api.point.InstanceMethodPointCut;
import org.apache.shenyu.agent.api.point.ShenyuAgentJoinPoint;
import org.apache.shenyu.agent.api.point.StaticMethodPointCut;
import org.apache.shenyu.agent.core.bytebuddy.interceptor.ConstructorInterceptor;
import org.apache.shenyu.agent.core.bytebuddy.interceptor.InstanceMethodInterceptor;
import org.apache.shenyu.agent.core.bytebuddy.interceptor.StaticMethodInterceptor;
import org.apache.shenyu.agent.core.bytebuddy.matcher.ShenyuAgentTypeMatcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The type Shenyu agent transformer.
 */
public final class ShenyuAgentTransformer implements Transformer {
    
    private static final String EXTRA_DATA = "_$EXTRA_DATA$_";
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuAgentTransformer.class);
    
    private static final ShenyuAgentTypeMatcher MATCHER = ShenyuAgentTypeMatcher.getInstance();
    
    @Override
    public Builder<?> transform(final Builder<?> builder, final TypeDescription typeDescription, final ClassLoader classLoader, final JavaModule module) {
        if (!MATCHER.containsType(typeDescription)) {
            return builder;
        }
        Builder<?> result = builder.defineField(EXTRA_DATA, Object.class, Opcodes.ACC_PRIVATE | Opcodes.ACC_VOLATILE).implement(TargetObject.class).intercept(FieldAccessor.ofField(EXTRA_DATA));
        ShenyuAgentJoinPoint joinPoint = MATCHER.loadShenyuAgentJoinPoint(typeDescription);
        result = interceptorConstructorPoint(typeDescription, joinPoint.getConstructorPoints(), result);
        result = interceptorStaticMethodPoint(typeDescription, joinPoint.getStaticMethodPoints(), result);
        result = interceptorInstanceMethodPoint(typeDescription, joinPoint.getInstanceMethodPoints(), result);
        return result;
    }
    
    private Builder<?> interceptorConstructorPoint(final TypeDescription description, final Collection<ConstructorPointCut> constructorPoints, final Builder<?> builder) {
        Collection<ShenyuAgentTransformerPoint<? extends ConstructorInterceptor>> constructorAdviceComposePoints = description.getDeclaredMethods().stream()
                .filter(MethodDescription::isConstructor)
                .map(each -> buildConstructorTransformerPoint(constructorPoints, each))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        final Builder<?>[] result = {builder};
        constructorAdviceComposePoints.forEach(point -> {
            try {
                result[0] = builder.constructor(ElementMatchers.is(point.getDescription()))
                        .intercept(SuperMethodCall.INSTANCE.andThen(MethodDelegation.withDefaultConfiguration()
                                .to(point.getInterceptor())));
                // CHECKSTYLE:OFF
            } catch (final Throwable ex) {
                // CHECKSTYLE:ON
                LOG.error("Failed to load handler class: {}", description.getTypeName(), ex);
            }
        });
        return result[0];
    }
    
    private ShenyuAgentTransformerPoint<? extends ConstructorInterceptor> buildConstructorTransformerPoint(
            final Collection<ConstructorPointCut> constructorPoints, final InDefinedShape methodDescription) {
        List<ConstructorPointCut> constructorPointCutList = constructorPoints.stream().filter(each -> each.getMatcher().matches(methodDescription)).collect(Collectors.toList());
        if (constructorPointCutList.isEmpty()) {
            return null;
        }
        List<ConstructorHandler> handlers = constructorPointCutList.stream()
                .flatMap(pointCut -> pointCut.getHandlers().stream())
                .map(handler -> (ConstructorHandler) MATCHER.getOrCreateInstance(handler))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        return new ShenyuAgentTransformerPoint<>(methodDescription, new ConstructorInterceptor(handlers));
    }
    
    private Builder<?> interceptorStaticMethodPoint(final TypeDescription description, final Collection<StaticMethodPointCut> pointCuts, final Builder<?> builder) {
        Collection<ShenyuAgentTransformerPoint<?>> points = description.getDeclaredMethods().stream()
                .filter(each -> each.isStatic() && !(each.isAbstract() || each.isSynthetic()))
                .map(methodDescription -> buildStaticMethodTransformationPoint(pointCuts, methodDescription))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        return getBuilder(description, builder, points);
    }
    
    private static Builder<?> getBuilder(final TypeDescription description, final Builder<?> builder, final Collection<ShenyuAgentTransformerPoint<?>> points) {
        final Builder<?>[] result = {builder};
        points.forEach(point -> {
            try {
                result[0] = builder.method(ElementMatchers.is(point.getDescription()))
                        .intercept(MethodDelegation.withDefaultConfiguration().to(point.getInterceptor()));
                // CHECKSTYLE:OFF
            } catch (final Throwable ex) {
                // CHECKSTYLE:ON
                LOG.error("Failed to load handler class: {}", description.getTypeName(), ex);
            }
        });
        return result[0];
    }
    
    private ShenyuAgentTransformerPoint<?> buildStaticMethodTransformationPoint(final Collection<StaticMethodPointCut> pointCuts, final InDefinedShape methodDescription) {
        List<StaticMethodPointCut> staticMethodPoints = pointCuts.stream().filter(point -> point.getMatcher().matches(methodDescription)).collect(Collectors.toList());
        if (staticMethodPoints.isEmpty()) {
            return null;
        }
        List<StaticMethodHandler> handlers = staticMethodPoints.stream()
                .flatMap(pointCut -> pointCut.getHandlers().stream())
                .map(handler -> (StaticMethodHandler) MATCHER.getOrCreateInstance(handler))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        return new ShenyuAgentTransformerPoint<>(methodDescription, new StaticMethodInterceptor(handlers));
    }
    
    private Builder<?> interceptorInstanceMethodPoint(final TypeDescription description, final Collection<InstanceMethodPointCut> pointCuts, final Builder<?> builder) {
        Collection<ShenyuAgentTransformerPoint<?>> points = description.getDeclaredMethods().stream()
                .filter(each -> !(each.isAbstract() || each.isSynthetic()))
                .map(each -> buildInstanceMethodTransformationPoint(pointCuts, each))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        return getBuilder(description, builder, points);
    }
    
    private ShenyuAgentTransformerPoint<?> buildInstanceMethodTransformationPoint(final Collection<InstanceMethodPointCut> pointCuts, final InDefinedShape methodDescription) {
        List<InstanceMethodPointCut> points = pointCuts.stream().filter(point -> point.getMatcher().matches(methodDescription)).collect(Collectors.toList());
        if (points.isEmpty()) {
            return null;
        }
        List<InstanceMethodHandler> handlers = points.stream()
                .flatMap(pointCut -> pointCut.getHandlers().stream())
                .map(handler -> (InstanceMethodHandler) MATCHER.getOrCreateInstance(handler))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        return new ShenyuAgentTransformerPoint<>(methodDescription, new InstanceMethodInterceptor(handlers));
    }
}
