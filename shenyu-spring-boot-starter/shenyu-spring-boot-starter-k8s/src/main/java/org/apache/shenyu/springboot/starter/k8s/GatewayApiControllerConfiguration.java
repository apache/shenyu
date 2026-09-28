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
import io.kubernetes.client.extended.controller.builder.ControllerBuilder;
import io.kubernetes.client.extended.controller.builder.DefaultControllerBuilder;
import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.controller.reconciler.Reconciler;
import io.kubernetes.client.extended.workqueue.DefaultRateLimitingQueue;
import io.kubernetes.client.extended.workqueue.RateLimitingQueue;
import io.kubernetes.client.extended.workqueue.WorkQueue;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.SharedInformerFactory;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsList;
import io.kubernetes.client.openapi.models.V1Service;
import io.kubernetes.client.openapi.models.V1ServiceList;
import io.kubernetes.client.util.generic.GenericKubernetesApi;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesApi;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.PluginRoleEnum;
import org.apache.shenyu.k8s.cache.K8sCacheReadiness;
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.apache.shenyu.k8s.common.GatewayApiCrdVersions;
import org.apache.shenyu.k8s.common.GatewayApiCrdVerifier;
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.apache.shenyu.k8s.reconciler.GatewayClassReconciler;
import org.apache.shenyu.k8s.reconciler.GatewayReconciler;
import org.apache.shenyu.k8s.reconciler.HTTPRouteReconciler;
import org.apache.shenyu.k8s.reconciler.HttpRouteEndpointsHandler;
import org.apache.shenyu.k8s.reconciler.HttpRouteServiceHandler;
import org.apache.shenyu.k8s.reconciler.ReferenceGrantReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.apache.shenyu.plugin.base.cache.CommonDiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.global.subsciber.MetaDataCacheSubscriber;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.SmartLifecycle;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.core.env.Environment;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Spring Boot auto-configuration for the Kubernetes Gateway API controller mode: the
 * bootstrap embeds the controller and writes parsed selector/rule config directly into the
 * in-process {@code BaseDataCache}. Multiple replicas need no leader election —
 * reconciliation is idempotent — but deployments MUST gate readiness on
 * {@code k8sCacheReadiness} so a cold pod receives no traffic before its informers sync.
 */
@Configuration
@ConditionalOnProperty(name = "shenyu.k8s.mode", havingValue = "gateway-api")
public class GatewayApiControllerConfiguration {

    /** Resync re-drives watch gaps (e.g. a grant added after ResolvedRefs=False); withResyncPeriod is a no-op in client-java. */
    private static final long RESYNC_PERIOD_MILLIS = Duration.ofMinutes(1).toMillis();

    private static final int DEFAULT_SERVER_PORT = 9195;

    /** One factory per resource type: DynamicKubernetesObject class keys collide in a shared factory. */
    @Bean("gatewayclass-shared-informer-factory")
    public SharedInformerFactory gatewayClassSharedInformerFactory(final ApiClient apiClient) {
        SharedInformerFactory factory = new SharedInformerFactory(apiClient);
        DynamicKubernetesApi gatewayClassApi = new DynamicKubernetesApi(
                GatewayApiConstants.GATEWAY_API_GROUP,
                GatewayApiConstants.GATEWAY_API_VERSION,
                "gatewayclasses",
                apiClient);
        factory.sharedIndexInformerFor(gatewayClassApi, DynamicKubernetesObject.class, RESYNC_PERIOD_MILLIS);
        return factory;
    }

    @Bean("gateway-shared-informer-factory")
    public SharedInformerFactory gatewaySharedInformerFactory(final ApiClient apiClient) {
        SharedInformerFactory factory = new SharedInformerFactory(apiClient);
        DynamicKubernetesApi gatewayApi = new DynamicKubernetesApi(
                GatewayApiConstants.GATEWAY_API_GROUP,
                GatewayApiConstants.GATEWAY_API_VERSION,
                "gateways",
                apiClient);
        factory.sharedIndexInformerFor(gatewayApi, DynamicKubernetesObject.class, RESYNC_PERIOD_MILLIS);
        return factory;
    }

    /** HTTPRoute, Service and Endpoints informers; Services map backendRef ports to named targetPorts. */
    @Bean("httproute-shared-informer-factory")
    public SharedInformerFactory httpRouteSharedInformerFactory(final ApiClient apiClient) {
        SharedInformerFactory factory = new SharedInformerFactory(apiClient);
        DynamicKubernetesApi httpRouteApi = new DynamicKubernetesApi(
                GatewayApiConstants.GATEWAY_API_GROUP,
                GatewayApiConstants.GATEWAY_API_VERSION,
                "httproutes",
                apiClient);
        factory.sharedIndexInformerFor(httpRouteApi, DynamicKubernetesObject.class, RESYNC_PERIOD_MILLIS);

        GenericKubernetesApi<V1Service, V1ServiceList> serviceApi = new GenericKubernetesApi<>(V1Service.class,
                V1ServiceList.class, "", "v1", "services", apiClient);
        factory.sharedIndexInformerFor(serviceApi, V1Service.class, RESYNC_PERIOD_MILLIS);

        GenericKubernetesApi<V1Endpoints, V1EndpointsList> endpointsApi = new GenericKubernetesApi<>(V1Endpoints.class,
                V1EndpointsList.class, "", "v1", "endpoints", apiClient);
        factory.sharedIndexInformerFor(endpointsApi, V1Endpoints.class, RESYNC_PERIOD_MILLIS);
        return factory;
    }

    @Bean("referencegrant-shared-informer-factory")
    public SharedInformerFactory referenceGrantSharedInformerFactory(final ApiClient apiClient) {
        SharedInformerFactory factory = new SharedInformerFactory(apiClient);
        DynamicKubernetesApi referenceGrantApi = new DynamicKubernetesApi(
                GatewayApiConstants.GATEWAY_API_GROUP,
                GatewayApiConstants.GATEWAY_API_VERSION,
                "referencegrants",
                apiClient);
        factory.sharedIndexInformerFor(referenceGrantApi, DynamicKubernetesObject.class, 0);
        return factory;
    }

    @Bean(destroyMethod = "shutdown")
    public ExecutorService controllerExecutorService() {
        return Executors.newCachedThreadPool(r -> {
            Thread t = new Thread(r, "shenyu-k8s-controller");
            t.setDaemon(true);
            return t;
        });
    }

    @Bean("gatewayclass-controller-manager")
    public ControllerManager gatewayClassControllerManager(
            @Qualifier("gatewayclass-shared-informer-factory") final SharedInformerFactory gatewayClassFactory,
            @Qualifier("gatewayclass-controller") final Controller gatewayClassController) {
        return new ControllerManager(gatewayClassFactory, gatewayClassController);
    }

    @Bean("gateway-controller-manager")
    public ControllerManager gatewayControllerManager(
            @Qualifier("gateway-shared-informer-factory") final SharedInformerFactory gatewayFactory,
            @Qualifier("gateway-controller") final Controller gatewayController) {
        return new ControllerManager(gatewayFactory, gatewayController);
    }

    @Bean("httproute-controller-manager")
    @DependsOn({"httpRouteEndpointsHandler", "httpRouteServiceHandler"})
    public ControllerManager httpRouteControllerManager(
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("httproute-controller") final Controller httpRouteController) {
        return new ControllerManager(httpRouteFactory, httpRouteController);
    }

    /** Re-queues HTTPRoutes referencing the grant's namespace so a revoked grant stops traffic immediately. */
    @Bean("referencegrant-controller")
    public Controller referenceGrantController(
            @Qualifier("referencegrant-shared-informer-factory") final SharedInformerFactory referenceGrantFactory,
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("httproute-work-queue") final RateLimitingQueue<Request> httpRouteWorkQueue) {
        DefaultControllerBuilder builder = ControllerBuilder.defaultBuilder(referenceGrantFactory);
        builder = builder.watch(q -> ControllerBuilder.controllerWatchBuilder(DynamicKubernetesObject.class, q)
                .build());
        builder.withWorkerCount(1);
        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer =
                httpRouteFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        Reconciler reconciler = new ReferenceGrantReconciler(httpRouteInformer, httpRouteWorkQueue);
        return builder.withReconciler(reconciler).withName("referenceGrantController").build();
    }

    @Bean("referencegrant-controller-manager")
    public ControllerManager referenceGrantControllerManager(
            @Qualifier("referencegrant-shared-informer-factory") final SharedInformerFactory referenceGrantFactory,
            @Qualifier("referencegrant-controller") final Controller referenceGrantController) {
        return new ControllerManager(referenceGrantFactory, referenceGrantController);
    }

    /** Fails fast when required CRDs are missing; the result feeds the SupportedVersion condition. */
    @Bean
    public GatewayApiCrdVersions gatewayApiCrdVersions(final ApiClient apiClient) {
        return GatewayApiCrdVerifier.verify(apiClient);
    }

    @Bean
    public SmartLifecycle k8sControllerLifecycle(final List<ControllerManager> controllerManagers,
                                                 final ExecutorService controllerExecutorService) {
        return new ControllerManagerLifecycle(controllerManagers, controllerExecutorService);
    }

    @Bean("gatewayclass-controller")
    public Controller gatewayClassController(
            @Qualifier("gatewayclass-shared-informer-factory") final SharedInformerFactory gatewayClassFactory,
            @Qualifier("gatewayclass-work-queue") final RateLimitingQueue<Request> gatewayClassWorkQueue,
            final GatewayClassReconciler gatewayClassReconciler) {
        DefaultControllerBuilder builder = ControllerBuilder.defaultBuilder(gatewayClassFactory)
                .withWorkQueue(gatewayClassWorkQueue);
        builder = builder.watch(q -> ControllerBuilder.controllerWatchBuilder(DynamicKubernetesObject.class, q)
                .build());
        builder.withWorkerCount(1);
        return builder.withReconciler(gatewayClassReconciler).withName("gatewayClassController").build();
    }

    @Bean("gateway-controller")
    public Controller gatewayController(
            @Qualifier("gateway-shared-informer-factory") final SharedInformerFactory gatewayFactory,
            final GatewayReconciler gatewayReconciler) {
        DefaultControllerBuilder builder = ControllerBuilder.defaultBuilder(gatewayFactory);
        builder = builder.watch(q -> ControllerBuilder.controllerWatchBuilder(DynamicKubernetesObject.class, q)
                .build());
        builder.withWorkerCount(2);
        return builder.withReconciler(gatewayReconciler).withName("gatewayController").build();
    }

    /** Also fed by the Gateway reconciler on accept/delete so finalizer updates are immediate. */
    @Bean("gatewayclass-work-queue")
    public RateLimitingQueue<Request> gatewayClassWorkQueue(final ExecutorService controllerExecutorService) {
        return new DefaultRateLimitingQueue<>(controllerExecutorService);
    }

    @Bean("httproute-work-queue")
    public RateLimitingQueue<Request> httpRouteWorkQueue(final ExecutorService controllerExecutorService) {
        return new DefaultRateLimitingQueue<>(controllerExecutorService);
    }

    @Bean("httproute-controller")
    public Controller httpRouteController(
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            final HTTPRouteReconciler httpRouteReconciler,
            @Qualifier("httproute-work-queue") final RateLimitingQueue<Request> httpRouteWorkQueue) {
        DefaultControllerBuilder builder = ControllerBuilder.defaultBuilder(httpRouteFactory)
                .withWorkQueue(httpRouteWorkQueue);
        builder = builder.watch(q -> ControllerBuilder.controllerWatchBuilder(DynamicKubernetesObject.class, q)
                .build());
        builder.withWorkerCount(2);
        return builder.withReconciler(httpRouteReconciler).withName("httpRouteController").build();
    }

    /** Enqueues routes whose backend Service Endpoints changed; a manager dependency so indexers register first. */
    @Bean
    public HttpRouteEndpointsHandler httpRouteEndpointsHandler(
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("httproute-work-queue") final RateLimitingQueue<Request> httpRouteWorkQueue) {
        SharedIndexInformer<V1Endpoints> endpointsInformer =
                httpRouteFactory.getExistingSharedIndexInformer(V1Endpoints.class);
        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer =
                httpRouteFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        HttpRouteEndpointsHandler handler = new HttpRouteEndpointsHandler(httpRouteInformer, httpRouteWorkQueue);
        endpointsInformer.addEventHandler(handler);
        return handler;
    }

    /** Service port/targetPort edits do not touch Endpoints, so they need their own trigger. */
    @Bean
    public HttpRouteServiceHandler httpRouteServiceHandler(
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("httproute-work-queue") final RateLimitingQueue<Request> httpRouteWorkQueue) {
        SharedIndexInformer<V1Service> serviceInformer =
                httpRouteFactory.getExistingSharedIndexInformer(V1Service.class);
        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer =
                httpRouteFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        HttpRouteServiceHandler handler = new HttpRouteServiceHandler(httpRouteInformer, httpRouteWorkQueue);
        serviceInformer.addEventHandler(handler);
        return handler;
    }

    @Bean
    public GatewayClassReconciler gatewayClassReconciler(
            @Qualifier("gatewayclass-shared-informer-factory") final SharedInformerFactory gatewayClassFactory,
            @Qualifier("gateway-shared-informer-factory") final SharedInformerFactory gatewayFactory,
            @Qualifier("gateway-controller") final Controller gatewayController,
            final ApiClient apiClient,
            final GatewayApiCrdVersions gatewayApiCrdVersions) {
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer =
                gatewayClassFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer =
                gatewayFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        RateLimitingQueue<Request> gatewayWorkQueue = ((DefaultController) gatewayController).getWorkQueue();
        return new GatewayClassReconciler(gatewayClassInformer, gatewayInformer, gatewayWorkQueue, apiClient, gatewayApiCrdVersions);
    }

    @Bean
    public GatewayReconciler gatewayReconciler(
            @Qualifier("gateway-shared-informer-factory") final SharedInformerFactory gatewayFactory,
            @Qualifier("gatewayclass-shared-informer-factory") final SharedInformerFactory gatewayClassFactory,
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("httproute-controller") final Controller httpRouteController,
            @Qualifier("gatewayclass-work-queue") final RateLimitingQueue<Request> gatewayClassWorkQueue,
            final ShenyuCacheRepository shenyuCacheRepository,
            final ApiClient apiClient,
            final Environment environment) {
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer =
                gatewayFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer =
                gatewayClassFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer =
                httpRouteFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        RateLimitingQueue<Request> httpRouteWorkQueue = ((DefaultController) httpRouteController).getWorkQueue();
        int servedPort = environment.getProperty("server.port", Integer.class, DEFAULT_SERVER_PORT);
        return new GatewayReconciler(gatewayInformer, gatewayClassInformer, httpRouteInformer,
                shenyuCacheRepository, httpRouteWorkQueue, gatewayClassWorkQueue, apiClient, servedPort);
    }

    @Bean
    public HTTPRouteReconciler httpRouteReconciler(
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("gateway-shared-informer-factory") final SharedInformerFactory gatewayFactory,
            @Qualifier("gatewayclass-shared-informer-factory") final SharedInformerFactory gatewayClassFactory,
            final HttpRouteParser httpRouteParser,
            final ShenyuCacheRepository shenyuCacheRepository,
            final ApiClient apiClient,
            final Environment environment) {
        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer =
                httpRouteFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer =
                gatewayFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer =
                gatewayClassFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        int servedPort = environment.getProperty("server.port", Integer.class, DEFAULT_SERVER_PORT);
        return new HTTPRouteReconciler(httpRouteInformer, gatewayInformer, gatewayClassInformer,
                httpRouteParser, shenyuCacheRepository, apiClient, servedPort);
    }

    @Bean
    public HttpRouteParser httpRouteParser(
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("referencegrant-shared-informer-factory") final SharedInformerFactory referenceGrantFactory) {
        SharedIndexInformer<V1Service> serviceInformer =
                httpRouteFactory.getExistingSharedIndexInformer(V1Service.class);
        SharedIndexInformer<V1Endpoints> endpointsInformer =
                httpRouteFactory.getExistingSharedIndexInformer(V1Endpoints.class);
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer =
                referenceGrantFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        Lister<V1Service> serviceLister = new Lister<>(serviceInformer.getIndexer());
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsInformer.getIndexer());
        Lister<DynamicKubernetesObject> referenceGrantLister = new Lister<>(referenceGrantInformer.getIndexer());
        return new HttpRouteParser(endpointsLister, serviceLister, referenceGrantLister);
    }

    @Bean
    public ShenyuCacheRepository shenyuCacheRepository(final CommonPluginDataSubscriber pluginDataSubscriber,
                                                       final CommonDiscoveryUpstreamDataSubscriber discoveryUpstreamDataSubscriber,
                                                       final MetaDataCacheSubscriber metaDataSubscriber) {
        ShenyuCacheRepository repository = new ShenyuCacheRepository(pluginDataSubscriber, discoveryUpstreamDataSubscriber,
                metaDataSubscriber, metaDataSubscriber);
        enablePlugin(repository, PluginEnum.GLOBAL, null);
        enablePlugin(repository, PluginEnum.URI, null);
        enablePlugin(repository, PluginEnum.NETTY_HTTP_CLIENT, null);
        enablePlugin(repository, PluginEnum.DIVIDE, "{multiSelectorHandle: 1, multiRuleHandle:0}");
        return repository;
    }

    /** Readiness needs informer sync AND drained work queues; the grant queue matters because its reconcile re-queues routes. */
    @Bean
    public K8sCacheReadiness k8sCacheReadiness(
            @Qualifier("gatewayclass-shared-informer-factory") final SharedInformerFactory gatewayClassFactory,
            @Qualifier("gateway-shared-informer-factory") final SharedInformerFactory gatewayFactory,
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("referencegrant-shared-informer-factory") final SharedInformerFactory referenceGrantFactory,
            @Qualifier("gatewayclass-controller") final Controller gatewayClassController,
            @Qualifier("gateway-controller") final Controller gatewayController,
            @Qualifier("referencegrant-controller") final Controller referenceGrantController,
            @Qualifier("httproute-work-queue") final RateLimitingQueue<Request> httpRouteWorkQueue) {
        List<SharedIndexInformer<?>> informers = new ArrayList<>();
        informers.add(gatewayClassFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class));
        informers.add(gatewayFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class));
        informers.add(httpRouteFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class));
        informers.add(httpRouteFactory.getExistingSharedIndexInformer(V1Service.class));
        informers.add(httpRouteFactory.getExistingSharedIndexInformer(V1Endpoints.class));
        informers.add(referenceGrantFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class));
        if (informers.stream().anyMatch(Objects::isNull)) {
            throw new IllegalStateException("Expected informer not registered; informer factory wiring is inconsistent");
        }
        List<WorkQueue<?>> workQueues = List.of(
                ((DefaultController) gatewayClassController).getWorkQueue(),
                ((DefaultController) gatewayController).getWorkQueue(),
                ((DefaultController) referenceGrantController).getWorkQueue(),
                httpRouteWorkQueue);
        return new K8sCacheReadiness(informers, workQueues);
    }

    private void enablePlugin(final ShenyuCacheRepository shenyuCacheRepository, final PluginEnum pluginEnum, final String config) {
        PluginData pluginData = PluginData.builder()
                .id(String.valueOf(pluginEnum.getCode()))
                .name(pluginEnum.getName())
                .config(config)
                .role(PluginRoleEnum.SYS.getName())
                .enabled(true)
                .sort(pluginEnum.getCode())
                .build();
        shenyuCacheRepository.saveOrUpdatePluginData(pluginData);
    }

    /**
     * Isolated from the CGLIB-proxied outer class (resolving an actuator @Bean without
     * actuator fails hard); repeats the mode condition because a nested @Configuration is
     * an independent scan candidate.
     */
    @Configuration
    @ConditionalOnProperty(name = "shenyu.k8s.mode", havingValue = "gateway-api")
    @ConditionalOnClass(name = "org.springframework.boot.actuate.health.HealthIndicator")
    static class HealthIndicatorConfiguration {

        @Bean
        public HealthIndicator k8sCacheReadinessHealthIndicator(final K8sCacheReadiness k8sCacheReadiness) {
            return () -> k8sCacheReadiness.isReady()
                    ? Health.up().withDetail("pendingInformers", 0L).withDetail("pendingWorkItems", 0L).build()
                    : Health.down().withDetail("pendingInformers", k8sCacheReadiness.pendingInformers())
                            .withDetail("pendingWorkItems", k8sCacheReadiness.pendingWorkItems()).build();
        }
    }
}
