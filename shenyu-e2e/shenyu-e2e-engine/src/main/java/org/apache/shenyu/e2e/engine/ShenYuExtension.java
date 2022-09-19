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

package org.apache.shenyu.e2e.engine;

import junit.framework.AssertionFailedError;
import org.apache.shenyu.e2e.annotation.ExternalService;
import org.apache.shenyu.e2e.annotation.ShenYuAdminClient;
import org.apache.shenyu.e2e.annotation.ShenYuGatewayClient;
import org.apache.shenyu.e2e.annotation.ShenYuInjectable;
import org.apache.shenyu.e2e.engine.annotation.ShenYuTest;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ConditionEvaluationResult;
import org.junit.jupiter.api.extension.ExecutionCondition;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ExtensionContext.Namespace;
import org.junit.jupiter.api.extension.ExtensionContext.Store;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolutionException;
import org.junit.jupiter.api.extension.ParameterResolver;
import org.junit.platform.commons.util.AnnotationUtils;
import org.testcontainers.DockerClientFactory;

import java.util.Objects;

public class ShenYuExtension implements BeforeAllCallback, ExecutionCondition, AfterAllCallback, ParameterResolver {
    private static final Namespace NAMESPACE = Namespace.create(ShenYuExtension.class);
    private static final String KEY_EXTENSION_CONTEXT = "_shenyu_service_compose_";
    private static final String KEY_ENGINE_CONFIGURE = "_shenyu_engine_configure_";
    
    @Override
    public void beforeAll(ExtensionContext extensionContext) throws Exception {
        Store store = extensionContext.getStore(NAMESPACE);
        
        ShenYuEngineConfigure configure = store.get(KEY_ENGINE_CONFIGURE, ShenYuEngineConfigure.class);
        ShenYuExtensionContext context = createExtensionContext(configure);
        context.setup();
        
        store.put(KEY_EXTENSION_CONTEXT, context);
    }
    
    @Override
    public ConditionEvaluationResult evaluateExecutionCondition(ExtensionContext context) {
        Store store = context.getStore(NAMESPACE);
        ShenYuEngineConfigure configure = store.get(KEY_ENGINE_CONFIGURE, ShenYuEngineConfigure.class);
        if (Objects.isNull(configure)) {
            Class<?> testClass = context.getTestClass().orElseThrow(() -> new AssertionFailedError("Test class not found"));
            if (!testClass.isAnnotationPresent(ShenYuTest.class)) {
                throw new AssertionFailedError("@ShenYuTest not found");
            }
            configure = ShenYuEngineConfigure.fromAnnotation(testClass.getAnnotation(ShenYuTest.class));
            store.put(KEY_ENGINE_CONFIGURE, configure);
        }
        
        if (configure.isRunOnDocker() && !isDockerAvailable()) {
            return ConditionEvaluationResult.disabled("Docker is not available");
        }
        
        return ConditionEvaluationResult.enabled("ShenYu test engine is available");
    }
    
    private boolean isDockerAvailable() {
        try {
            DockerClientFactory.instance().client();
            return true;
        } catch (Throwable ex) {
            return false;
        }
    }
    
    @Override
    public boolean supportsParameter(ParameterContext parameter, ExtensionContext extensionContext) throws ParameterResolutionException {
        return AnnotationUtils.isAnnotated(parameter.getParameter().getType(), ShenYuInjectable.class);
    }
    
    @Override
    public Object resolveParameter(ParameterContext parameter, ExtensionContext extensionContext) throws ParameterResolutionException {
        Store store = extensionContext.getStore(NAMESPACE);
        ShenYuExtensionContext context = store.get(KEY_EXTENSION_CONTEXT, ShenYuExtensionContext.class);
        
        Class<?> parameterType = parameter.getParameter().getType();
        if (parameterType.isAnnotationPresent(ShenYuAdminClient.class)) {
            return context.getAdminClient();
        }
        if (parameterType.isAnnotationPresent(ShenYuGatewayClient.class)) {
            return context.getGatewayClient();
        }
        ExternalService service = AnnotationUtils.findAnnotation(parameterType, ExternalService.class)
                .orElseThrow(() -> new AssertionFailedError("")); // fixme
        return context.getExternalServiceClient(service.serviceName());
    }
    
    @Override
    public void afterAll(ExtensionContext extensionContext) throws Exception {
        Store store = extensionContext.getStore(NAMESPACE);
        ShenYuExtensionContext context = store.get(KEY_EXTENSION_CONTEXT, ShenYuExtensionContext.class);
        Assertions.assertTrue(Objects.nonNull(context), "ShenYuExtensionContext is non-nullable");
        context.cleanup();
    }
    
    static ShenYuExtensionContext createExtensionContext(ShenYuEngineConfigure config) {
        return new ShenYuExtensionContext(config);
    }
}
