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

package org.apache.shenyu.e2e.engine.scenario;

import org.apache.shenyu.e2e.engine.annotation.ShenYuScenario;
import org.apache.shenyu.e2e.engine.scenario.specification.ScenarioSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ScenarioSpecLogProxy;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.extension.Extension;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ExtensionContext.Namespace;
import org.junit.jupiter.api.extension.ExtensionContext.Store;
import org.junit.jupiter.api.extension.TestTemplateInvocationContext;
import org.junit.jupiter.api.extension.TestTemplateInvocationContextProvider;
import org.junit.platform.commons.support.ReflectionSupport;

import java.lang.reflect.Method;
import java.util.List;
import java.util.stream.Stream;
import java.util.stream.Stream.Builder;

public class ShenYuScenarioInvocationContextProvider implements TestTemplateInvocationContextProvider {
    
    @Override
    public boolean supportsTestTemplate(ExtensionContext context) {
        final Method method = context.getRequiredTestMethod();
        if (!method.isAnnotationPresent(ShenYuScenario.class)) {
            return false;
        }
        ShenYuScenario scenario = method.getAnnotation(ShenYuScenario.class);
        Class<? extends ShenYuScenarioProvider> providerClass = scenario.provider();
        ShenYuScenarioProvider provider = ReflectionSupport.newInstance(providerClass);
        ShenYuScenarioSupplier supplier = new ShenYuScenarioSupplier(provider.get());
        
        Store store = getStore(context);
        store.put(ShenYuScenarioSupplier.class, supplier);
        return true;
    }
    
    @Override
    public Stream<TestTemplateInvocationContext> provideTestTemplateInvocationContexts(ExtensionContext context) {
        Store store = getStore(context);
        
        ShenYuScenarioSupplier supplier = store.get(ShenYuScenarioSupplier.class, ShenYuScenarioSupplier.class);
        Builder<TestTemplateInvocationContext> builder = Stream.builder();
        supplier.forEach(e -> builder.add(new ShenYuTestTemplateInvocationContext(e)));
        
        return builder.build();
    }
    
    private static Store getStore(ExtensionContext context) {
        return context.getStore(Namespace.create(ShenYuScenarioInvocationContextProvider.class, context.getRequiredTestMethod()));
    }
    
    private static class ShenYuTestTemplateInvocationContext implements TestTemplateInvocationContext {
        private final ScenarioSpec scenarioSpec;
        
        public ShenYuTestTemplateInvocationContext(ScenarioSpec scenarioSpec) {
            this.scenarioSpec = new ScenarioSpecLogProxy(scenarioSpec);
        }
        
        @Override
        public String getDisplayName(int invocationIndex) {
            return "#" + invocationIndex + " " + scenarioSpec.getName();
        }
        
        @Override
        public List<Extension> getAdditionalExtensions() {
            return Lists.newArrayList(new ShenYuScenarioParameterResolver(scenarioSpec));
        }
    }
    
}
