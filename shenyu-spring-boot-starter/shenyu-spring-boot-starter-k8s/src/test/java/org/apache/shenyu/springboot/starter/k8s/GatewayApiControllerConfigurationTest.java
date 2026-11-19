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

package org.apache.shenyu.springboot.starter.k8s;

import io.kubernetes.client.common.KubernetesObject;
import io.kubernetes.client.extended.controller.Controller;
import io.kubernetes.client.extended.controller.DefaultController;
import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.workqueue.DefaultRateLimitingQueue;
import io.kubernetes.client.extended.workqueue.RateLimitingQueue;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.SharedInformerFactory;
import io.kubernetes.client.informer.cache.Cache;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1Namespace;
import io.kubernetes.client.openapi.models.V1Service;
import io.kubernetes.client.util.generic.GenericKubernetesApi;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.k8s.cache.K8sCacheReadiness;
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.apache.shenyu.k8s.reconciler.GatewayReconciler;
import org.apache.shenyu.k8s.reconciler.HTTPRouteReconciler;
import org.apache.shenyu.k8s.reconciler.HttpRouteNamespaceHandler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.DependsOn;
import org.springframework.core.env.Environment;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutorService;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Isolated Gateway API bean wiring checks without starting informers or querying CRDs.
 */
public final class GatewayApiControllerConfigurationTest {

    private final GatewayApiControllerConfiguration configuration = new GatewayApiControllerConfiguration();

    private SharedInformerFactory httpRouteFactory;

    private SharedInformerFactory gatewayFactory;

    private SharedInformerFactory gatewayClassFactory;

    private SharedInformerFactory referenceGrantFactory;

    private SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer;

    private SharedIndexInformer<DynamicKubernetesObject> gatewayInformer;

    private SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer;

    private SharedIndexInformer<V1Namespace> namespaceInformer;

    @BeforeEach
    public void setUp() {
        httpRouteFactory = mock(SharedInformerFactory.class);
        gatewayFactory = mock(SharedInformerFactory.class);
        gatewayClassFactory = mock(SharedInformerFactory.class);
        referenceGrantFactory = mock(SharedInformerFactory.class);
        httpRouteInformer = informer();
        gatewayInformer = informer();
        gatewayClassInformer = informer();
        namespaceInformer = informer();
        when(httpRouteFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class)).thenReturn(httpRouteInformer);
        when(httpRouteFactory.getExistingSharedIndexInformer(V1Namespace.class)).thenReturn(namespaceInformer);
        SharedIndexInformer<V1Service> serviceInformer = informer();
        when(httpRouteFactory.getExistingSharedIndexInformer(V1Service.class)).thenReturn(serviceInformer);
        SharedIndexInformer<V1Endpoints> endpointsInformer = informer();
        when(httpRouteFactory.getExistingSharedIndexInformer(V1Endpoints.class)).thenReturn(endpointsInformer);
        when(gatewayFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class)).thenReturn(gatewayInformer);
        when(gatewayClassFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class)).thenReturn(gatewayClassInformer);
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer = informer();
        when(referenceGrantFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class)).thenReturn(referenceGrantInformer);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testHttpRouteFactoryRegistersTypedNamespaceInformer() {
        ApiClient apiClient = new ApiClient();
        try (MockedConstruction<SharedInformerFactory> factories = mockConstruction(SharedInformerFactory.class)) {
            SharedInformerFactory factory = configuration.httpRouteSharedInformerFactory(apiClient);

            Assertions.assertSame(factories.constructed().get(0), factory);
            verify(factory).sharedIndexInformerFor(any(GenericKubernetesApi.class), eq(V1Namespace.class), eq(60_000L));
            verify(factory).sharedIndexInformerFor(any(GenericKubernetesApi.class), eq(DynamicKubernetesObject.class), eq(60_000L));
            verify(factory).sharedIndexInformerFor(any(GenericKubernetesApi.class), eq(V1Service.class), eq(60_000L));
            verify(factory).sharedIndexInformerFor(any(GenericKubernetesApi.class), eq(V1Endpoints.class), eq(60_000L));
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testNamespaceHandlerRegistersBeforeManagerStartup() throws Exception {
        RateLimitingQueue<Request> queue = mock(RateLimitingQueue.class);

        HttpRouteNamespaceHandler handler = configuration.httpRouteNamespaceHandler(httpRouteFactory, queue);

        verify(namespaceInformer).addEventHandler(handler);
        Method managerMethod = GatewayApiControllerConfiguration.class.getMethod("httpRouteControllerManager", SharedInformerFactory.class, Controller.class);
        DependsOn dependsOn = managerMethod.getAnnotation(DependsOn.class);
        Assertions.assertNotNull(dependsOn);
        Assertions.assertTrue(Arrays.asList(dependsOn.value()).contains("httpRouteNamespaceHandler"));
    }

    @Test
    public void testReadinessWaitsForNamespaceInformer() {
        when(namespaceInformer.hasSynced()).thenReturn(false);
        K8sCacheReadiness readiness = readiness();

        Assertions.assertEquals(1, readiness.pendingInformers());
        Assertions.assertFalse(readiness.isReady());
        Assertions.assertFalse(readiness.isReady());

        when(namespaceInformer.hasSynced()).thenReturn(true);
        Assertions.assertEquals(0, readiness.pendingInformers());
        Assertions.assertFalse(readiness.isReady());
        Assertions.assertTrue(readiness.isReady());
    }

    @Test
    public void testReadinessRejectsMissingNamespaceInformer() {
        when(httpRouteFactory.getExistingSharedIndexInformer(V1Namespace.class)).thenReturn(null);

        Assertions.assertThrows(IllegalStateException.class, this::readiness);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testGatewayQueueSharedWithHttpRouteReconciler() {
        RateLimitingQueue<Request> gatewayQueue = mock(RateLimitingQueue.class);
        HttpRouteParser parser = mock(HttpRouteParser.class);
        ShenyuCacheRepository repository = mock(ShenyuCacheRepository.class);
        ApiClient apiClient = mock(ApiClient.class);
        Environment environment = mock(Environment.class);
        when(environment.getProperty("server.port", Integer.class, 9195)).thenReturn(9195);
        Controller gatewayController = configuration.gatewayController(gatewayFactory, gatewayQueue, mock(GatewayReconciler.class));

        Assertions.assertSame(gatewayQueue, ((DefaultController) gatewayController).getWorkQueue());
        try (MockedConstruction<HTTPRouteReconciler> reconcilers = mockConstruction(HTTPRouteReconciler.class, (reconciler, context) -> {
            Assertions.assertEquals(List.of(httpRouteInformer, gatewayInformer, gatewayClassInformer, namespaceInformer,
                    parser, repository, apiClient, 9195, gatewayQueue), context.arguments());
            Assertions.assertSame(((DefaultController) gatewayController).getWorkQueue(), context.arguments().get(8));
        })) {
            HTTPRouteReconciler reconciler = configuration.httpRouteReconciler(httpRouteFactory, gatewayFactory, gatewayClassFactory,
                    gatewayQueue, parser, repository, apiClient, environment);

            Assertions.assertSame(reconcilers.constructed().get(0), reconciler);
        }
    }

    @Test
    public void testGatewayQueueBeanUsesSharedExecutor() {
        ExecutorService executor = mock(ExecutorService.class);
        try (MockedConstruction<DefaultRateLimitingQueue> queues = mockConstruction(DefaultRateLimitingQueue.class, (queue, context) ->
                Assertions.assertSame(executor, context.arguments().get(0)))) {
            RateLimitingQueue<Request> queue = configuration.gatewayWorkQueue(executor);

            Assertions.assertSame(queues.constructed().get(0), queue);
        }
    }

    @Test
    public void testGatewayQueueDependenciesUseQualifier() throws Exception {
        Method controllerMethod = GatewayApiControllerConfiguration.class.getMethod("gatewayController",
                SharedInformerFactory.class, RateLimitingQueue.class, GatewayReconciler.class);
        Method routeMethod = GatewayApiControllerConfiguration.class.getMethod("httpRouteReconciler",
                SharedInformerFactory.class, SharedInformerFactory.class, SharedInformerFactory.class, RateLimitingQueue.class,
                HttpRouteParser.class, ShenyuCacheRepository.class, ApiClient.class, Environment.class);

        Assertions.assertEquals("gateway-work-queue", controllerMethod.getParameters()[1].getAnnotation(Qualifier.class).value());
        Assertions.assertEquals("gateway-work-queue", routeMethod.getParameters()[3].getAnnotation(Qualifier.class).value());
    }

    @SuppressWarnings("unchecked")
    private <T extends KubernetesObject> SharedIndexInformer<T> informer() {
        SharedIndexInformer<T> informer = mock(SharedIndexInformer.class);
        when(informer.getIndexer()).thenReturn(new Cache<>());
        when(informer.hasSynced()).thenReturn(true);
        return informer;
    }

    @SuppressWarnings("unchecked")
    private K8sCacheReadiness readiness() {
        RateLimitingQueue<Request> queue = mock(RateLimitingQueue.class);
        DefaultController controller = mock(DefaultController.class);
        when(controller.getWorkQueue()).thenReturn(queue);
        return configuration.k8sCacheReadiness(gatewayClassFactory, gatewayFactory, httpRouteFactory, referenceGrantFactory,
                controller, controller, controller, queue);
    }
}
