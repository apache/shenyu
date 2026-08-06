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
import io.kubernetes.client.extended.controller.LeaderElectingController;
import io.kubernetes.client.extended.controller.builder.ControllerBuilder;
import io.kubernetes.client.extended.controller.builder.DefaultControllerBuilder;
import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.leaderelection.LeaderElectionConfig;
import io.kubernetes.client.extended.leaderelection.LeaderElector;
import io.kubernetes.client.extended.leaderelection.Lock;
import io.kubernetes.client.extended.leaderelection.resourcelock.LeaseLock;
import io.kubernetes.client.extended.workqueue.RateLimitingQueue;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.SharedInformerFactory;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsList;
import io.kubernetes.client.util.generic.GenericKubernetesApi;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesApi;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.PluginRoleEnum;
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.apache.shenyu.k8s.reconciler.GatewayClassReconciler;
import org.apache.shenyu.k8s.reconciler.GatewayReconciler;
import org.apache.shenyu.k8s.reconciler.HTTPRouteReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.apache.shenyu.plugin.base.cache.CommonDiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.global.subsciber.MetaDataCacheSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Configuration
@ConditionalOnProperty(name = "shenyu.k8s.mode", havingValue = "gateway-api")
public class GatewayApiControllerConfiguration {

    private static final Logger LOG = LoggerFactory.getLogger(GatewayApiControllerConfiguration.class);

    /**
     * Lock name shared by all ShenYu Gateway API controller replicas for leader election.
     * All three controllers (gatewayclass/gateway/httproute) contend on the same Lease so
     * only one replica reconciles at a time.
     */
    private static final String LEADER_ELECTION_LOCK_NAME = "shenyu-k8s-gateway-api";

    /**
     * Default namespace for the leader-election Lease when POD_NAMESPACE is not injected
     * (e.g. running outside a Pod in dev). Production deployments must inject POD_NAMESPACE
     * via the Kubernetes Downward API.
     */
    private static final String DEFAULT_LEADER_ELECTION_NAMESPACE = "default";

    /**
     * Default identity used in the Lease when POD_NAME is not injected. Each replica must
     * have a distinct identity for leader election to work correctly.
     */
    private static final String DEFAULT_LEADER_ELECTION_IDENTITY = "shenyu-0";

    /**
     * GatewayClass SharedInformerFactory - only registers GatewayClass informer.
     * Separate factory to avoid DynamicKubernetesObject class key collision.
     *
     * @param apiClient the Kubernetes API client
     * @return the SharedInformerFactory for GatewayClass resources
     */
    @Bean("gatewayclass-shared-informer-factory")
    public SharedInformerFactory gatewayClassSharedInformerFactory(final ApiClient apiClient) {
        SharedInformerFactory factory = new SharedInformerFactory(apiClient);
        DynamicKubernetesApi gatewayClassApi = new DynamicKubernetesApi(
                GatewayApiConstants.GATEWAY_API_GROUP,
                GatewayApiConstants.GATEWAY_API_VERSION,
                "gatewayclasses",
                apiClient);
        factory.sharedIndexInformerFor(gatewayClassApi, DynamicKubernetesObject.class, 0);
        return factory;
    }

    /**
     * Gateway SharedInformerFactory - only registers Gateway informer.
     * Separate from other factories to avoid DynamicKubernetesObject class key collision.
     *
     * @param apiClient the Kubernetes API client
     * @return the SharedInformerFactory for Gateway resources
     */
    @Bean("gateway-shared-informer-factory")
    public SharedInformerFactory gatewaySharedInformerFactory(final ApiClient apiClient) {
        SharedInformerFactory factory = new SharedInformerFactory(apiClient);
        DynamicKubernetesApi gatewayApi = new DynamicKubernetesApi(
                GatewayApiConstants.GATEWAY_API_GROUP,
                GatewayApiConstants.GATEWAY_API_VERSION,
                "gateways",
                apiClient);
        factory.sharedIndexInformerFor(gatewayApi, DynamicKubernetesObject.class, 0);
        return factory;
    }

    /**
     * HTTPRoute SharedInformerFactory - registers HTTPRoute and Endpoints informers.
     * Separate from gatewayFactory to avoid DynamicKubernetesObject class key collision.
     *
     * @param apiClient the Kubernetes API client
     * @return the SharedInformerFactory for HTTPRoute and Endpoints resources
     */
    @Bean("httproute-shared-informer-factory")
    public SharedInformerFactory httpRouteSharedInformerFactory(final ApiClient apiClient) {
        SharedInformerFactory factory = new SharedInformerFactory(apiClient);
        DynamicKubernetesApi httpRouteApi = new DynamicKubernetesApi(
                GatewayApiConstants.GATEWAY_API_GROUP,
                GatewayApiConstants.GATEWAY_API_VERSION,
                "httproutes",
                apiClient);
        factory.sharedIndexInformerFor(httpRouteApi, DynamicKubernetesObject.class, 0);

        GenericKubernetesApi<V1Endpoints, V1EndpointsList> endpointsApi = new GenericKubernetesApi<>(V1Endpoints.class,
                V1EndpointsList.class, "", "v1", "endpoints", apiClient);
        factory.sharedIndexInformerFor(endpointsApi, V1Endpoints.class, 0);
        return factory;
    }

    /**
     * Shared ExecutorService for all ControllerManager beans, with a destroy method to
     * ensure graceful shutdown and prevent thread leaks on context close.
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
            @Qualifier("gatewayclass-controller") final Controller gatewayClassController,
            final ApiClient apiClient,
            final ExecutorService controllerExecutorService,
            @Value("${shenyu.k8s.leader-election.enabled:true}") final boolean leaderElectionEnabled,
            @Value("${POD_NAMESPACE:default}") final String podNamespace,
            @Value("${POD_NAME:shenyu-0}") final String podName) {
        Controller controller = maybeWrapLeaderElection(apiClient, gatewayClassController,
                leaderElectionEnabled, podNamespace, podName);
        ControllerManager controllerManager = new ControllerManager(gatewayClassFactory, controller);
        controllerExecutorService.submit(controllerManager);
        return controllerManager;
    }

    @Bean("gateway-controller-manager")
    public ControllerManager gatewayControllerManager(
            @Qualifier("gateway-shared-informer-factory") final SharedInformerFactory gatewayFactory,
            @Qualifier("gateway-controller") final Controller gatewayController,
            final ApiClient apiClient,
            final ExecutorService controllerExecutorService,
            @Value("${shenyu.k8s.leader-election.enabled:true}") final boolean leaderElectionEnabled,
            @Value("${POD_NAMESPACE:default}") final String podNamespace,
            @Value("${POD_NAME:shenyu-0}") final String podName) {
        Controller controller = maybeWrapLeaderElection(apiClient, gatewayController,
                leaderElectionEnabled, podNamespace, podName);
        ControllerManager controllerManager = new ControllerManager(gatewayFactory, controller);
        controllerExecutorService.submit(controllerManager);
        return controllerManager;
    }

    @Bean("httproute-controller-manager")
    public ControllerManager httpRouteControllerManager(
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("httproute-controller") final Controller httpRouteController,
            final ApiClient apiClient,
            final ExecutorService controllerExecutorService,
            @Value("${shenyu.k8s.leader-election.enabled:true}") final boolean leaderElectionEnabled,
            @Value("${POD_NAMESPACE:default}") final String podNamespace,
            @Value("${POD_NAME:shenyu-0}") final String podName) {
        Controller controller = maybeWrapLeaderElection(apiClient, httpRouteController,
                leaderElectionEnabled, podNamespace, podName);
        ControllerManager controllerManager = new ControllerManager(httpRouteFactory, controller);
        controllerExecutorService.submit(controllerManager);
        return controllerManager;
    }

    /**
     * Optionally wrap a controller with leader election via a Kubernetes Lease. When enabled
     * (default), only the replica holding the Lease runs the controller; the others block
     * waiting to acquire it. This prevents multi-replica deployments from double-reconciling
     * and racing on status patches.
     *
     * <p>Disable via {@code shenyu.k8s.leader-election.enabled=false} for single-replica or
     * dev setups where the RBAC for {@code coordination.k8s.io/leases} is not available.
     *
     * @param apiClient            the Kubernetes API client
     * @param delegate             the controller to guard with leader election
     * @param leaderElectionEnabled whether to enable leader election
     * @param podNamespace         namespace of the Lease (injected via Downward API in prod)
     * @param podName              holder identity written into the Lease (injected via Downward API)
     * @return the original controller, or a leader-electing wrapper
     */
    private Controller maybeWrapLeaderElection(final ApiClient apiClient, final Controller delegate,
                                               final boolean leaderElectionEnabled,
                                               final String podNamespace, final String podName) {
        if (!leaderElectionEnabled) {
            LOG.info("Leader election disabled for [{}], running as standalone controller", delegate);
            return delegate;
        }
        String namespace = (Objects.isNull(podNamespace) || podNamespace.isEmpty()) ? DEFAULT_LEADER_ELECTION_NAMESPACE : podNamespace;
        String identity = (Objects.isNull(podName) || podName.isEmpty()) ? DEFAULT_LEADER_ELECTION_IDENTITY : podName;
        Lock lock = new LeaseLock(namespace, LEADER_ELECTION_LOCK_NAME, identity, apiClient);
        // leaseDuration: how long before a lease expires
        // renewDeadline: how long the leader tries to renew before giving up
        // retryPeriod: interval between renew attempts
        LeaderElectionConfig config = new LeaderElectionConfig(
                lock,
                Duration.ofSeconds(15),
                Duration.ofSeconds(10),
                Duration.ofSeconds(2));
        LOG.info("Leader election enabled for [{}] via Lease {}/{} (identity={})",
                delegate, namespace, LEADER_ELECTION_LOCK_NAME, identity);
        return new LeaderElectingController(new LeaderElector(config), delegate);
    }

    @Bean("gatewayclass-controller")
    public Controller gatewayClassController(
            @Qualifier("gatewayclass-shared-informer-factory") final SharedInformerFactory gatewayClassFactory,
            final GatewayClassReconciler gatewayClassReconciler) {
        DefaultControllerBuilder builder = ControllerBuilder.defaultBuilder(gatewayClassFactory);
        builder = builder.watch(q -> ControllerBuilder.controllerWatchBuilder(DynamicKubernetesObject.class, q)
                .withResyncPeriod(Duration.ofMinutes(1))
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
                .withResyncPeriod(Duration.ofMinutes(1))
                .build());
        builder.withWorkerCount(2);
        return builder.withReconciler(gatewayReconciler).withName("gatewayController").build();
    }

    @Bean("httproute-controller")
    public Controller httpRouteController(
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            final HTTPRouteReconciler httpRouteReconciler) {
        DefaultControllerBuilder builder = ControllerBuilder.defaultBuilder(httpRouteFactory);
        builder = builder.watch(q -> ControllerBuilder.controllerWatchBuilder(DynamicKubernetesObject.class, q)
                .withResyncPeriod(Duration.ofMinutes(1))
                .build());
        builder.withWorkerCount(2);
        return builder.withReconciler(httpRouteReconciler).withName("httpRouteController").build();
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
            final ApiClient apiClient) {
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer =
                gatewayFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer =
                gatewayClassFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer =
                httpRouteFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        RateLimitingQueue<Request> httpRouteWorkQueue = ((DefaultController) httpRouteController).getWorkQueue();
        return new GatewayReconciler(gatewayInformer, gatewayClassInformer, httpRouteInformer,
                shenyuCacheRepository, httpRouteWorkQueue, apiClient);
    }

    @Bean
    public HTTPRouteReconciler httpRouteReconciler(
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory,
            @Qualifier("gateway-shared-informer-factory") final SharedInformerFactory gatewayFactory,
            @Qualifier("gatewayclass-shared-informer-factory") final SharedInformerFactory gatewayClassFactory,
            final HttpRouteParser httpRouteParser,
            final ShenyuCacheRepository shenyuCacheRepository,
            final ApiClient apiClient) {
        SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer =
                httpRouteFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer =
                gatewayFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer =
                gatewayClassFactory.getExistingSharedIndexInformer(DynamicKubernetesObject.class);
        return new HTTPRouteReconciler(httpRouteInformer, gatewayInformer, gatewayClassInformer,
                httpRouteParser, shenyuCacheRepository, apiClient);
    }

    @Bean
    public HttpRouteParser httpRouteParser(
            @Qualifier("httproute-shared-informer-factory") final SharedInformerFactory httpRouteFactory) {
        SharedIndexInformer<V1Endpoints> endpointsInformer =
                httpRouteFactory.getExistingSharedIndexInformer(V1Endpoints.class);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsInformer.getIndexer());
        return new HttpRouteParser(endpointsLister);
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
}
