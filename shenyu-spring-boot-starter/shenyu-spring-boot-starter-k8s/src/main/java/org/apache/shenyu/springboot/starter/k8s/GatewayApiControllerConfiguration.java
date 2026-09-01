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
import org.apache.shenyu.k8s.common.GatewayApiCrdVerifier;
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.apache.shenyu.k8s.reconciler.GatewayClassReconciler;
import org.apache.shenyu.k8s.reconciler.GatewayReconciler;
import org.apache.shenyu.k8s.reconciler.HTTPRouteReconciler;
import org.apache.shenyu.k8s.reconciler.HttpRouteEndpointsHandler;
import org.apache.shenyu.k8s.reconciler.ReferenceGrantReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.apache.shenyu.plugin.base.cache.CommonDiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.global.subsciber.MetaDataCacheSubscriber;
import org.springframework.beans.factory.SmartInitializingSingleton;
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
 * Spring Boot auto-configuration for the Kubernetes Gateway API controller mode.
 *
 * <p>The ShenYu bootstrap embeds the Kubernetes controller and serves as both control plane
 * and data plane in a single JVM: the controller watches Gateway API resources and writes
 * the parsed selector/rule config directly into the in-process {@code BaseDataCache}.
 *
 * <p><b>Multiple replicas supported</b> without leader election: reconciliation is idempotent
 * (selector/rule IDs are derived deterministically from the route coordinates, so every
 * replica converges to the same cache) and status patches are skipped when unchanged.
 * Deployments MUST use a readiness probe gated on the {@code k8sCacheReadiness} health
 * indicator so a cold pod receives no traffic before its informers finish the initial sync.
 */
@Configuration
@ConditionalOnProperty(name = "shenyu.k8s.mode", havingValue = "gateway-api")
public class GatewayApiControllerConfiguration {

    /**
     * Informer resync period. Periodic resync re-drives changes not covered by watches
     * (e.g. a ReferenceGrant added after a route reported ResolvedRefs=False) and retries
     * failed status patches. Set here because ControllerWatch's withResyncPeriod is a
     * no-op in client-java.
     */
    private static final long RESYNC_PERIOD_MILLIS = Duration.ofMinutes(1).toMillis();

    /** Fallback for server.port; matches the bootstrap default. */
    private static final int DEFAULT_SERVER_PORT = 9195;

    /**
     * GatewayClass informer factory; separate factories avoid DynamicKubernetesObject class-key collisions.
     *
     * @param apiClient the Kubernetes API client
     * @return the GatewayClass SharedInformerFactory
     */
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

    /**
     * HTTPRoute, Service and Endpoints informer factory. Services are watched to map a
     * backendRef's Service port to its targetPort (including named targetPorts), which the
     * Endpoints alone cannot express for multi-port Services.
     *
     * @param apiClient the Kubernetes API client
     * @return the HTTPRoute, Service and Endpoints SharedInformerFactory
     */
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

    /**
     * ReferenceGrant informer factory. Grants live in the namespace of the referenced
     * resource; the informer is consumed read-only for cross-namespace validation.
     *
     * @param apiClient the Kubernetes API client
     * @return the ReferenceGrant SharedInformerFactory
     */
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

    /**
     * Shared executor for all controller managers, with graceful shutdown on context close.
     *
     * @return daemon cached thread pool executor
     */
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
    @DependsOn("httpRouteEndpointsHandler")
    public ControllerManager httpRouteControllerManager(
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("httproute-controller") final Controller httpRouteController) {
        return new ControllerManager(httpRouteFactory, httpRouteController);
    }

    /**
     * ReferenceGrant controller: re-queues HTTPRoutes referencing the grant's namespace, so
     * grant changes take effect immediately instead of on the next route resync (a revoked
     * grant must stop unauthorized traffic right away).
     *
     * @param referenceGrantFactory the ReferenceGrant SharedInformerFactory
     * @param httpRouteFactory the HTTPRoute SharedInformerFactory
     * @param httpRouteWorkQueue the HTTPRoute controller work queue
     * @return the ReferenceGrant controller
     */
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

    /**
     * Fail fast when the cluster does not serve the required Gateway API CRDs at v1,
     * before any informer starts crash-looping on 404s. Runs after all singletons are
     * instantiated and before the controller lifecycle starts the informers.
     *
     * @param apiClient the Kubernetes API client
     * @return the startup check
     */
    @Bean
    public SmartInitializingSingleton gatewayApiCrdVerifier(final ApiClient apiClient) {
        return () -> GatewayApiCrdVerifier.verify(apiClient);
    }

    /**
     * Start all controller managers after context refresh (and stop them on close);
     * see {@link ControllerManagerLifecycle}.
     *
     * @param controllerManagers all controller managers of this mode
     * @param controllerExecutorService the shared controller executor
     * @return the lifecycle driving the controllers
     */
    @Bean
    public SmartLifecycle k8sControllerLifecycle(final List<ControllerManager> controllerManagers,
                                                 final ExecutorService controllerExecutorService) {
        return new ControllerManagerLifecycle(controllerManagers, controllerExecutorService);
    }

    @Bean("gatewayclass-controller")
    public Controller gatewayClassController(
            @Qualifier("gatewayclass-shared-informer-factory") final SharedInformerFactory gatewayClassFactory,
            final GatewayClassReconciler gatewayClassReconciler) {
        DefaultControllerBuilder builder = ControllerBuilder.defaultBuilder(gatewayClassFactory);
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

    /**
     * Shared work queue for the HTTPRoute controller, also fed by the Endpoints handler.
     *
     * @param controllerExecutorService the shared controller executor
     * @return the HTTPRoute controller work queue
     */
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

    /**
     * Enqueues HTTPRoutes whose backendRefs target a changed Service. Declared as a
     * dependency of the HTTPRoute controller manager so its indexers are registered
     * before the informers start.
     *
     * @param httpRouteFactory the HTTPRoute and Endpoints SharedInformerFactory
     * @param httpRouteWorkQueue the HTTPRoute controller work queue
     * @return the registered Endpoints event handler
     */
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

    @Bean
    public GatewayClassReconciler gatewayClassReconciler(
            @Qualifier("gatewayclass-shared-informer-factory") final SharedInformerFactory gatewayClassFactory,
            @Qualifier("gateway-shared-informer-factory") final SharedInformerFactory gatewayFactory,
            @Qualifier("gateway-controller") final Controller gatewayController,
            final ApiClient apiClient) {
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer =
                gatewayClassFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer =
                gatewayFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        RateLimitingQueue<Request> gatewayWorkQueue = ((DefaultController) gatewayController).getWorkQueue();
        return new GatewayClassReconciler(gatewayClassInformer, gatewayInformer, gatewayWorkQueue, apiClient);
    }

    @Bean
    public GatewayReconciler gatewayReconciler(
            @Qualifier("gateway-shared-informer-factory") final SharedInformerFactory gatewayFactory,
            @Qualifier("gatewayclass-shared-informer-factory") final SharedInformerFactory gatewayClassFactory,
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("httproute-controller") final Controller httpRouteController,
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
                shenyuCacheRepository, httpRouteWorkQueue, apiClient, servedPort);
    }

    @Bean
    public HTTPRouteReconciler httpRouteReconciler(
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("gateway-shared-informer-factory") final SharedInformerFactory gatewayFactory,
            @Qualifier("gatewayclass-shared-informer-factory") final SharedInformerFactory gatewayClassFactory,
            @Qualifier("referencegrant-shared-informer-factory") final SharedInformerFactory referenceGrantFactory,
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
        SharedIndexInformer<DynamicKubernetesObject> referenceGrantInformer =
                referenceGrantFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        int servedPort = environment.getProperty("server.port", Integer.class, DEFAULT_SERVER_PORT);
        return new HTTPRouteReconciler(httpRouteInformer, gatewayInformer, gatewayClassInformer,
                referenceGrantInformer, httpRouteParser, shenyuCacheRepository, apiClient, servedPort);
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
                                                       final MetaDataCacheSubscriber metaDataSubscriber,
                                                       final MetaDataCacheSubscriber metaDataCacheSubscriber) {
        ShenyuCacheRepository repository = new ShenyuCacheRepository(pluginDataSubscriber, discoveryUpstreamDataSubscriber, metaDataSubscriber, metaDataCacheSubscriber);
        enablePlugin(repository, PluginEnum.GLOBAL, null);
        enablePlugin(repository, PluginEnum.URI, null);
        enablePlugin(repository, PluginEnum.NETTY_HTTP_CLIENT, null);
        enablePlugin(repository, PluginEnum.DIVIDE, "{multiSelectorHandle: 1, multiRuleHandle:0}");
        enablePlugin(repository, PluginEnum.GENERAL_CONTEXT, null);
        return repository;
    }

    /**
     * Readiness aggregator over all registered informers and the controller work queues of
     * this mode: informer sync alone does not mean the objects were reconciled into the
     * local cache yet, so the queues' initial backlog must drain too.
     *
     * @param gatewayClassFactory the GatewayClass SharedInformerFactory
     * @param gatewayFactory the Gateway SharedInformerFactory
     * @param httpRouteFactory the HTTPRoute, Service and Endpoints SharedInformerFactory
     * @param referenceGrantFactory the ReferenceGrant SharedInformerFactory
     * @param gatewayClassController the GatewayClass controller (for its work queue)
     * @param gatewayController the Gateway controller (for its work queue)
     * @param httpRouteWorkQueue the HTTPRoute controller work queue
     * @return readiness aggregator over all registered informers and work queues
     */
    @Bean
    public K8sCacheReadiness k8sCacheReadiness(
            @Qualifier("gatewayclass-shared-informer-factory") final SharedInformerFactory gatewayClassFactory,
            @Qualifier("gateway-shared-informer-factory") final SharedInformerFactory gatewayFactory,
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("referencegrant-shared-informer-factory") final SharedInformerFactory referenceGrantFactory,
            @Qualifier("gatewayclass-controller") final Controller gatewayClassController,
            @Qualifier("gateway-controller") final Controller gatewayController,
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
     * Isolated nested configuration for the actuator health indicator: the outer class is
     * CGLIB-proxied and would resolve every {@code @Bean} method signature — a hard failure
     * when actuator is absent. Must repeat the outer mode condition: a static nested
     * {@code @Configuration} class is an independent component-scan candidate and
     * {@code ShenyuConfiguration}'s broad scan would otherwise register it even when the
     * outer configuration is skipped (e.g. in ingress mode).
     */
    @Configuration
    @ConditionalOnProperty(name = "shenyu.k8s.mode", havingValue = "gateway-api")
    @ConditionalOnClass(name = "org.springframework.boot.actuate.health.HealthIndicator")
    static class HealthIndicatorConfiguration {

        /**
         * Exposes {@link K8sCacheReadiness} as a health indicator: include
         * {@code k8sCacheReadiness} in the readiness group and point the probe at
         * {@code /actuator/health/readiness}.
         *
         * @param k8sCacheReadiness the informer/reconciliation readiness aggregator
         * @return health indicator reflecting informer initial-sync and backlog state
         */
        @Bean
        public HealthIndicator k8sCacheReadinessHealthIndicator(final K8sCacheReadiness k8sCacheReadiness) {
            return () -> k8sCacheReadiness.isReady()
                    ? Health.up().withDetail("pendingInformers", 0L).withDetail("pendingWorkItems", 0L).build()
                    : Health.down().withDetail("pendingInformers", k8sCacheReadiness.pendingInformers())
                            .withDetail("pendingWorkItems", k8sCacheReadiness.pendingWorkItems()).build();
        }
    }
}
