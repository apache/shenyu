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
import io.kubernetes.client.extended.controller.builder.ControllerBuilder;
import io.kubernetes.client.extended.controller.builder.DefaultControllerBuilder;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.SharedInformerFactory;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsList;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressList;
import io.kubernetes.client.openapi.models.V1Secret;
import io.kubernetes.client.openapi.models.V1SecretList;
import io.kubernetes.client.openapi.models.V1Service;
import io.kubernetes.client.openapi.models.V1ServiceList;
import io.kubernetes.client.util.generic.GenericKubernetesApi;
import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.common.config.NettyHttpProperties;
import org.apache.shenyu.common.config.ssl.ShenyuSniAsyncMapping;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.k8s.parser.IngressParser;
import org.apache.shenyu.k8s.reconciler.EndpointsReconciler;
import org.apache.shenyu.k8s.reconciler.IngressReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.apache.shenyu.plugin.base.cache.CommonDiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.global.subsciber.MetaDataCacheSubscriber;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import reactor.netty.tcp.TcpSslContextSpec;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.time.Duration;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.Executors;

/**
 * The type shenyu ingress controller configuration.
 */
@Configuration
public class IngressControllerConfiguration {

    /**
     * Controller Manager.
     *
     * @param sharedInformerFactory sharedInformerFactory
     * @param ingressController ingressController
     * @param endpointsController endpointsController
     * @return Controller Manager
     */
    @Bean("controller-manager")
    public ControllerManager controllerManager(final SharedInformerFactory sharedInformerFactory,
                                               @Qualifier("ingress-controller") final Controller ingressController,
                                               @Qualifier("endpoints-controller") final Controller endpointsController) {
        ControllerManager controllerManager = new ControllerManager(sharedInformerFactory, ingressController, endpointsController);
        Executors.newSingleThreadExecutor().submit(controllerManager);
        return controllerManager;
    }

    /**
     * Ingress Controller.
     *
     * @param sharedInformerFactory sharedInformerFactory
     * @param ingressReconciler ingressReconciler
     * @return Ingress Controller
     */
    @Bean("ingress-controller")
    public Controller ingressController(final SharedInformerFactory sharedInformerFactory, final IngressReconciler ingressReconciler) {
        DefaultControllerBuilder builder = ControllerBuilder.defaultBuilder(sharedInformerFactory);
        builder = builder.watch(q -> ControllerBuilder.controllerWatchBuilder(V1Ingress.class, q)
                                .withResyncPeriod(Duration.ofMinutes(1))
                                .build());
        // TODO support config in application.yaml
        builder.withWorkerCount(2);
        return builder.withReconciler(ingressReconciler).withName("ingressController").build();
    }

    /**
     * Ingress Reconciler.
     *
     * @param ingressInformer ingress shared informer
     * @param secretInformer secret shared informer
     * @param shenyuCacheRepository ShenyuCacheRepository
     * @param shenyuSniAsyncMappingProvider shenyuSniAsyncMappingProvider
     * @param ingressParser IngressParser
     * @param apiClient ApiClient
     * @return Ingress Reconciler
     */
    @Bean
    public IngressReconciler ingressReconciler(final SharedIndexInformer<V1Ingress> ingressInformer,
                                               final SharedIndexInformer<V1Secret> secretInformer,
                                               final ShenyuCacheRepository shenyuCacheRepository,
                                               final ObjectProvider<ShenyuSniAsyncMapping> shenyuSniAsyncMappingProvider,
                                               final IngressParser ingressParser,
                                               final ApiClient apiClient) {
        ShenyuSniAsyncMapping shenyuSniAsyncMapping = Optional.ofNullable(shenyuSniAsyncMappingProvider.getIfAvailable()).orElse(new ShenyuSniAsyncMapping());
        return new IngressReconciler(ingressInformer, secretInformer, shenyuCacheRepository, shenyuSniAsyncMapping, ingressParser, apiClient);
    }

    /**
     * Endpoints Controller.
     *
     * @param sharedInformerFactory sharedInformerFactory
     * @param endpointsReconciler endpointsReconciler
     * @return Endpoints Controller
     */
    @Bean("endpoints-controller")
    public Controller endpointsController(final SharedInformerFactory sharedInformerFactory, final EndpointsReconciler endpointsReconciler) {
        DefaultControllerBuilder builder = ControllerBuilder.defaultBuilder(sharedInformerFactory);
        builder = builder.watch(q -> ControllerBuilder.controllerWatchBuilder(V1Endpoints.class, q)
                .withResyncPeriod(Duration.ofMinutes(1))
                .build());
        builder.withWorkerCount(2);
        return builder.withReconciler(endpointsReconciler).withName("ingressController").build();
    }

    /**
     * EndpointsReconciler.
     *
     * @param ingressInformer ingressInformer
     * @param endpointsInformer endpointsInformer
     * @param shenyuCacheRepository shenyuCacheRepository
     * @param apiClient apiClient
     * @return EndpointsReconciler
     */
    @Bean
    public EndpointsReconciler endpointsReconciler(final SharedIndexInformer<V1Ingress> ingressInformer,
                                                   final SharedIndexInformer<V1Endpoints> endpointsInformer,
                                                   final ShenyuCacheRepository shenyuCacheRepository,
                                                   final ApiClient apiClient) {
        return new EndpointsReconciler(ingressInformer, endpointsInformer, shenyuCacheRepository, apiClient);
    }

    /**
     * ShenyuCacheRepository.
     *
     * @param pluginDataSubscriber CommonPluginDataSubscriber
     * @param discoveryUpstreamDataSubscriber CommonDiscoveryUpstreamDataSubscriber
     * @param metaDataSubscriber MetaDataSubscriber
     * @param metaDataCacheSubscriber MetaDataCacheSubscriber
     * @return ShenyuCacheRepository
     */
    @Bean
    public ShenyuCacheRepository shenyuCacheRepository(final CommonPluginDataSubscriber pluginDataSubscriber,
                                                       final CommonDiscoveryUpstreamDataSubscriber discoveryUpstreamDataSubscriber,
                                                       final MetaDataCacheSubscriber metaDataSubscriber,
                                                       final MetaDataCacheSubscriber metaDataCacheSubscriber) {
        return new ShenyuCacheRepository(pluginDataSubscriber, discoveryUpstreamDataSubscriber, metaDataSubscriber, metaDataCacheSubscriber);
    }

    /**
     * IngressParser.
     *
     * @param serviceInformer serviceInformer
     * @param endpointsInformer endpointsInformer
     * @return IngressParser
     */
    @Bean
    public IngressParser ingressParser(final SharedIndexInformer<V1Service> serviceInformer, final SharedIndexInformer<V1Endpoints> endpointsInformer) {
        return new IngressParser(serviceInformer, endpointsInformer);
    }

    /**
     * ServiceInformer.
     *
     * @param apiClient apiClient
     * @param sharedInformerFactory sharedInformerFactory
     * @return serviceInformer
     */
    @Bean
    public SharedIndexInformer<V1Service> serviceInformer(final ApiClient apiClient, final SharedInformerFactory sharedInformerFactory) {
        GenericKubernetesApi<V1Service, V1ServiceList> genericApi = new GenericKubernetesApi<>(V1Service.class,
                V1ServiceList.class, "", "v1", "services", apiClient);
        return sharedInformerFactory.sharedIndexInformerFor(genericApi, V1Service.class, 0);
    }

    /**
     * EndpointsInformer.
     *
     * @param apiClient apiClient
     * @param sharedInformerFactory sharedInformerFactory
     * @return endpointsInformer
     */
    @Bean
    public SharedIndexInformer<V1Endpoints> endpointsInformer(final ApiClient apiClient, final SharedInformerFactory sharedInformerFactory) {
        GenericKubernetesApi<V1Endpoints, V1EndpointsList> genericApi = new GenericKubernetesApi<>(V1Endpoints.class,
                V1EndpointsList.class, "", "v1", "endpoints", apiClient);
        return sharedInformerFactory.sharedIndexInformerFor(genericApi, V1Endpoints.class, 0);
    }

    /**
     * SecretInformer.
     *
     * @param apiClient apiClient
     * @param sharedInformerFactory sharedInformerFactory
     * @return secretInformer
     */
    @Bean
    public SharedIndexInformer<V1Secret> secretInformer(final ApiClient apiClient, final SharedInformerFactory sharedInformerFactory) {
        GenericKubernetesApi<V1Secret, V1SecretList> genericApi = new GenericKubernetesApi<>(V1Secret.class,
                V1SecretList.class, "", "v1", "secrets", apiClient);
        return sharedInformerFactory.sharedIndexInformerFor(genericApi, V1Secret.class, 0);
    }

    /**
     * IngressInformer.
     *
     * @param apiClient apiClient
     * @param sharedInformerFactory sharedInformerFactory
     * @return ingressInformer
     */
    @Bean
    public SharedIndexInformer<V1Ingress> ingressInformer(final ApiClient apiClient, final SharedInformerFactory sharedInformerFactory) {
        GenericKubernetesApi<V1Ingress, V1IngressList> genericApi = new GenericKubernetesApi<>(V1Ingress.class,
                V1IngressList.class, "networking.k8s.io", "v1", "ingresses", apiClient);
        return sharedInformerFactory.sharedIndexInformerFor(genericApi, V1Ingress.class, 0);
    }

    /**
     * TcpSslContextSpec.
     *
     * @param properties NettyHttpProperties
     * @param apiClient ApiClient
     * @return TcpSslContextSpec
     * @throws ApiException the exception when use apiClient directly
     */
    @Bean
    @ConditionalOnProperty(value = {"shenyu.netty.http.web-server-factory-enabled", "shenyu.netty.http.sni.enabled"}, havingValue = "true")
    public TcpSslContextSpec tcpSslContextSpec(final ObjectProvider<NettyHttpProperties> properties, final ApiClient apiClient) throws ApiException {
        NettyHttpProperties nettyHttpProperties = Optional.ofNullable(properties.getIfAvailable()).orElse(new NettyHttpProperties());
        NettyHttpProperties.SniProperties sniProperties = nettyHttpProperties.getSni();
        if (Objects.nonNull(sniProperties) && sniProperties.getEnabled() && "k8s".equals(sniProperties.getMod())) {
            String defaultName = Optional.ofNullable(sniProperties.getDefaultK8sSecretName()).orElse("default-ingress-crt");
            String defaultNamespace = Optional.ofNullable(sniProperties.getDefaultK8sSecretNamespace()).orElse("default");
            CoreV1Api coreV1Api = new CoreV1Api(apiClient);
            V1Secret secret = coreV1Api.readNamespacedSecret(defaultName, defaultNamespace, "true");
            Map<String, byte[]> secretData = secret.getData();
            if (MapUtils.isEmpty(secretData)) {
                InputStream crtStream = new ByteArrayInputStream(secretData.get("tls.crt"));
                InputStream keyStream = new ByteArrayInputStream(secretData.get("tls.key"));
                return TcpSslContextSpec.forServer(crtStream, keyStream);
            } else {
                throw new ShenyuException(String.format("Can not read cert and key from default secret %s/%s", defaultNamespace, defaultName));
            }
        }
        return TcpSslContextSpec.forServer(new ByteArrayInputStream(new byte[]{}), new ByteArrayInputStream(new byte[]{}));
    }
}
