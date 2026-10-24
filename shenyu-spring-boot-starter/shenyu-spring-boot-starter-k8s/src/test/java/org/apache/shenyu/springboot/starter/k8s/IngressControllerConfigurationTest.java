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

import io.kubernetes.client.extended.controller.Controller;
import io.kubernetes.client.extended.controller.ControllerManager;
import io.kubernetes.client.extended.controller.DefaultController;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.SharedInformerFactory;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1Ingress;
import org.apache.shenyu.k8s.reconciler.EndpointsReconciler;
import org.apache.shenyu.k8s.reconciler.IngressReconciler;
import org.junit.jupiter.api.Test;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.SmartLifecycle;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link IngressControllerConfiguration}.
 */
public final class IngressControllerConfigurationTest {

    private final IngressControllerConfiguration configuration = new IngressControllerConfiguration();

    @Test
    public void testDefaultProperties() {
        ShenyuKubernetesControllerProperties properties = new ShenyuKubernetesControllerProperties();

        assertThat(properties.getWorkerCount(), is(2));
        assertThat(properties.getResyncPeriod(), is(Duration.ofMinutes(1)));
    }

    @Test
    public void testPropertiesBinding() {
        new ApplicationContextRunner()
                .withUserConfiguration(PropertiesConfiguration.class)
                .withPropertyValues(
                        "shenyu.k8s.controller.worker-count=4",
                        "shenyu.k8s.controller.resync-period=30s")
                .run(context -> {
                    ShenyuKubernetesControllerProperties properties = context.getBean(ShenyuKubernetesControllerProperties.class);
                    assertThat(properties.getWorkerCount(), is(4));
                    assertThat(properties.getResyncPeriod(), is(Duration.ofSeconds(30)));
                });
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testControllersUseConfiguredTuning() {
        SharedInformerFactory sharedInformerFactory = mock(SharedInformerFactory.class);
        SharedIndexInformer<V1Ingress> ingressInformer = mock(SharedIndexInformer.class);
        SharedIndexInformer<V1Endpoints> endpointsInformer = mock(SharedIndexInformer.class);
        when(sharedInformerFactory.getExistingSharedIndexInformer(V1Ingress.class)).thenReturn(ingressInformer);
        when(sharedInformerFactory.getExistingSharedIndexInformer(V1Endpoints.class)).thenReturn(endpointsInformer);
        ShenyuKubernetesControllerProperties properties = new ShenyuKubernetesControllerProperties();
        properties.setWorkerCount(4);
        properties.setResyncPeriod(Duration.ofSeconds(30));

        Controller ingressController = configuration.ingressController(sharedInformerFactory, mock(IngressReconciler.class), properties);
        Controller endpointsController = configuration.endpointsController(sharedInformerFactory, mock(EndpointsReconciler.class), properties);

        assertThat(ingressController, instanceOf(DefaultController.class));
        assertThat(endpointsController, instanceOf(DefaultController.class));
        assertThat(((DefaultController) ingressController).getWorkerCount(), is(4));
        assertThat(((DefaultController) endpointsController).getWorkerCount(), is(4));
        verify(ingressInformer).addEventHandlerWithResyncPeriod(any(), eq(30_000L));
        verify(endpointsInformer).addEventHandlerWithResyncPeriod(any(), eq(30_000L));
    }

    @Test
    public void testControllerLifecycleAndExecutor() throws Exception {
        ControllerManager controllerManager = mock(ControllerManager.class);
        ExecutorService executorService = configuration.ingressControllerExecutorService();
        try {
            SmartLifecycle lifecycle = configuration.ingressControllerLifecycle(controllerManager, executorService);
            assertThat(lifecycle, instanceOf(ControllerManagerLifecycle.class));
            assertThat(executorService.submit(() -> Thread.currentThread().getName()).get(5, TimeUnit.SECONDS),
                    is("shenyu-k8s-ingress-controller"));

            lifecycle.start();
            lifecycle.stop();

            verify(controllerManager).run();
            verify(controllerManager).shutdown();
        } finally {
            executorService.shutdownNow();
        }
    }

    @Test
    public void testLifecycleDoesNotStartDuringCreation() {
        ControllerManager controllerManager = mock(ControllerManager.class);
        ExecutorService executorService = mock(ExecutorService.class);

        configuration.ingressControllerLifecycle(controllerManager, executorService);

        verifyNoInteractions(controllerManager, executorService);
    }

    @Configuration
    @EnableConfigurationProperties(ShenyuKubernetesControllerProperties.class)
    static class PropertiesConfiguration {
    }
}
