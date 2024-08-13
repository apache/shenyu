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

import org.apache.shenyu.e2e.annotation.ExternalService;
import org.apache.shenyu.e2e.annotation.ShenYuAdminClient;
import org.apache.shenyu.e2e.annotation.ShenYuGatewayClient;
import org.apache.shenyu.e2e.annotation.ShenYuInjectable;
import org.apache.shenyu.e2e.config.ServiceConfigure;
import org.apache.shenyu.e2e.enums.ServiceTypeEnum;
import org.apache.shenyu.e2e.engine.annotation.ShenYuTest;
import org.apache.shenyu.e2e.engine.annotation.ShenYuTest.Environment;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure;
import org.apache.shenyu.e2e.engine.utils.SocketUtils;
import org.jetbrains.annotations.NotNull;
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
import org.opentest4j.AssertionFailedError;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class ShenYuExtension implements BeforeAllCallback, ExecutionCondition, AfterAllCallback, ParameterResolver {
    
    private static final Namespace NAMESPACE = Namespace.create(ShenYuExtension.class);
    
    private static final String KEY_EXTENSION_CONTEXT = "_shenyu_service_compose_";
    
    private static final String KEY_ENGINE_CONFIGURE = "_shenyu_engine_configure_";
    
    @Override
    public void beforeAll(final ExtensionContext extensionContext) throws Exception {
        Store store = extensionContext.getStore(NAMESPACE);
        ShenYuEngineConfigure configure = store.get(KEY_ENGINE_CONFIGURE, ShenYuEngineConfigure.class);
        ShenYuExtensionContext context = createExtensionContext(configure);
        store.put(KEY_EXTENSION_CONTEXT, context);
    }
    
    @Override
    public ConditionEvaluationResult evaluateExecutionCondition(final ExtensionContext context) {
        Store store = context.getStore(NAMESPACE);
        ShenYuEngineConfigure configure = store.get(KEY_ENGINE_CONFIGURE, ShenYuEngineConfigure.class);
        if (Objects.isNull(configure)) {
            Class<?> testClass = context.getTestClass().orElseThrow(() -> new AssertionFailedError("Test class not found"));
            final Environment[] environments = getEnvironments(testClass);
            Arrays.stream(environments).collect(Collectors.groupingBy(Environment::serviceName)).forEach((serviceName, environmentList) -> {
                if (environmentList.size() > 1) {
                    throw new AssertionFailedError("ShenYuE2ETest.Environment serviceName is duplicate");
                }
            });
            // FIXME check service is available
            for (ShenYuTest.Environment environment : environments) {
                if (!SocketUtils.checkUrl(environment.service().baseUrl(), 15000)) {
                    throw new AssertionFailedError(environment.serviceName() + ":" + environment.service().baseUrl() + " is not available");
                    //return ConditionEvaluationResult.disabled(environment.serviceName() + ":" + environment.service().baseUrl() + " is not available");
                }
            }
            configure = ShenYuEngineConfigure.fromAnnotation(testClass.getAnnotation(ShenYuTest.class));
            store.put(KEY_ENGINE_CONFIGURE, configure);
        }
        
        return ConditionEvaluationResult.enabled("enabled");
    }
    
    @Override
    public boolean supportsParameter(final ParameterContext parameterContext, final ExtensionContext extensionContext) throws ParameterResolutionException {
        return AnnotationUtils.isAnnotated(parameterContext.getParameter().getType(), ShenYuInjectable.class);
    }
    
    @Override
    public Object resolveParameter(final ParameterContext parameterContext, final ExtensionContext extensionContext) throws ParameterResolutionException {
        Store store = extensionContext.getStore(NAMESPACE);
        ShenYuExtensionContext context = store.get(KEY_EXTENSION_CONTEXT, ShenYuExtensionContext.class);
        ShenYuEngineConfigure configure = store.get(KEY_ENGINE_CONFIGURE, ShenYuEngineConfigure.class);
        Class<?> parameterType = parameterContext.getParameter().getType();
        if (parameterType.isAnnotationPresent(ShenYuAdminClient.class)) {
            List<ServiceConfigure> list = getServiceConfigureList(configure, ServiceTypeEnum.SHENYU_ADMIN);
            assertServiceConfigure(list);
            if (list.size() == 1) {
                ServiceConfigure serviceConfigure = list.stream().findFirst().orElseThrow(() -> new AssertionFailedError("ShenYuAdminClient not found"));
                return context.getAdminClientMap().get(serviceConfigure.getServiceName());
            }
        } else if (parameterType.isAnnotationPresent(ShenYuGatewayClient.class)) {
            List<ServiceConfigure> list = getServiceConfigureList(configure, ServiceTypeEnum.SHENYU_GATEWAY);
            assertServiceConfigure(list);
            if (list.size() == 1) {
                ServiceConfigure serviceConfigure = list.stream().findFirst().orElseThrow(() -> new AssertionFailedError("ShenYuAdminClient not found"));
                return context.getGatewayClientMap().get(serviceConfigure.getServiceName());
            }
        } else if (parameterType.isAnnotationPresent(ExternalService.class)) {
            List<ServiceConfigure> list = getServiceConfigureList(configure, ServiceTypeEnum.EXTERNAL_SERVICE);
            assertServiceConfigure(list);
            if (list.size() == 1) {
                ServiceConfigure serviceConfigure = list.stream().findFirst().orElseThrow(() -> new AssertionFailedError("ShenYuAdminClient not found"));
                return context.getExternalServiceClientMap().get(serviceConfigure.getServiceName());
            }
        }
        return context.getEnvironmentClient();
    }
    
    private List<ServiceConfigure> getServiceConfigureList(final ShenYuEngineConfigure configure, final ServiceTypeEnum serviceType) {
        return configure.getServiceConfigureMap().values().stream()
                .filter(serviceConfigure -> serviceType.equals(serviceConfigure.getServiceType()))
                .collect(Collectors.toList());
    }
    
    private void assertServiceConfigure(final List<ServiceConfigure> serviceConfigures) {
        if (serviceConfigures.isEmpty()) {
            throw new AssertionFailedError("ShenYu Client not found");
        }
    }
    
    @Override
    public void afterAll(final ExtensionContext extensionContext) throws Exception {
        Store store = extensionContext.getStore(NAMESPACE);
        ShenYuExtensionContext context = store.get(KEY_EXTENSION_CONTEXT, ShenYuExtensionContext.class);
        ShenYuEngineConfigure configure = store.get(KEY_ENGINE_CONFIGURE, ShenYuEngineConfigure.class);
        Assertions.assertTrue(Objects.nonNull(context), "ShenYuExtensionContext is non-nullable");
    }
    
    @NotNull
    private static Environment[] getEnvironments(final Class<?> testClass) {
        if (!testClass.isAnnotationPresent(ShenYuTest.class)) {
            throw new AssertionFailedError("@ShenYuE2ETest not found");
        }
        Environment[] environments = testClass.getAnnotation(ShenYuTest.class).environments();
        if (environments.length == 0) {
            throw new AssertionFailedError("@ShenYuE2ETest.Environment not found");
        }
        return environments;
    }
    
    private ShenYuExtensionContext createExtensionContext(final ShenYuEngineConfigure configure) {
        return new ShenYuExtensionContext(configure);
    }
}
