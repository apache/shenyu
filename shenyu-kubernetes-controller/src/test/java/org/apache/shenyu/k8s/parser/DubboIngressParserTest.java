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

package org.apache.shenyu.k8s.parser;

import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Indexer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubsetBuilder;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsBuilder;
import io.kubernetes.client.openapi.models.V1HTTPIngressPathBuilder;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.shenyu.common.dto.convert.selector.DubboUpstream;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.k8s.common.IngressConstants;
import io.kubernetes.client.openapi.models.V1ServiceBuilder;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test for DubboIngressParser upstream protocol parsing.
 */
public class DubboIngressParserTest {

    private static final String NAMESPACE = "test-namespace";

    private static final String SERVICE_NAME = "backend-service";


    private Lister<V1Service> serviceLister;

    private Indexer<V1Endpoints> endpointsIndexer;

    private Lister<V1Endpoints> endpointsLister;

    @BeforeEach
    @SuppressWarnings("unchecked")
    public void setUp() {
        serviceLister = new Lister<>(mock(Indexer.class));
        endpointsIndexer = mock(Indexer.class);
        endpointsLister = new Lister<>(endpointsIndexer);
    }

    private List<DubboUpstream> parseAndGetUpstreams(final Map<String, String> annotations) {
        V1Endpoints endpoints = new V1EndpointsBuilder()
                .withNewMetadata().withNamespace("test").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder()
                        .withAddresses(new V1EndpointAddress().ip("10.0.0.1"),
                                new V1EndpointAddress().ip("10.0.0.2"),
                                new V1EndpointAddress().ip("10.0.0.3"))
                        .build())
                .build();
        when(endpointsIndexer.getByKey("test/testService")).thenReturn(endpoints);

        Map<String, String> allAnnotations = new HashMap<>();
        allAnnotations.put("kubernetes.io/ingress.class", "shenyu");
        if (Objects.nonNull(annotations)) {
            allAnnotations.putAll(annotations);
        }
        Map<String, String> labels = metadataLabels();

        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("testIngress").withNamespace("test")
                .withAnnotations(allAnnotations).withLabels(labels).endMetadata()
                .withNewSpec().withRules(
                        new V1IngressRuleBuilder().withNewHttp().withPaths(
                                        new V1HTTPIngressPathBuilder().withPath("/test")
                                                .withNewBackend()
                                                .withNewService().withName("testService").withNewPort().withNumber(20880).endPort().endService()
                                                .endBackend().build())
                                .endHttp().build())
                .endSpec()
                .build();

        DubboIngressParser parser = new DubboIngressParser(serviceLister, endpointsLister);
        ShenyuMemoryConfig result = parser.parse(ingress, null);

        String handle = result.getRouteConfigList().get(0).getSelectorData().getHandle();
        return GsonUtils.getInstance().fromList(handle, DubboUpstream.class);
    }

    @Test
    public void testProtocolAnnotationMissing() {
        List<DubboUpstream> upstreams = assertDoesNotThrow(() -> parseAndGetUpstreams(null));
        assertEquals(3, upstreams.size());
        for (DubboUpstream upstream : upstreams) {
            assertEquals("dubbo://", upstream.getProtocol());
        }
    }

    @Test
    public void testProtocolAnnotationExactMatch() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.UPSTREAMS_PROTOCOL_ANNOTATION_KEY, "dubbo://,dubbo://,dubbo://");
        List<DubboUpstream> upstreams = assertDoesNotThrow(() -> parseAndGetUpstreams(annotations));
        assertEquals(3, upstreams.size());
        for (DubboUpstream upstream : upstreams) {
            assertEquals("dubbo://", upstream.getProtocol());
        }
    }

    @Test
    public void testProtocolAnnotationFewerThanAddresses() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.UPSTREAMS_PROTOCOL_ANNOTATION_KEY, "triple://");
        List<DubboUpstream> upstreams = assertDoesNotThrow(() -> parseAndGetUpstreams(annotations));
        assertEquals(3, upstreams.size());
        assertEquals("triple://", upstreams.get(0).getProtocol());
        assertEquals("dubbo://", upstreams.get(1).getProtocol());
        assertEquals("dubbo://", upstreams.get(2).getProtocol());
    }

    @Test
    public void testProtocolAnnotationMixed() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.UPSTREAMS_PROTOCOL_ANNOTATION_KEY, "triple://,dubbo://");
        List<DubboUpstream> upstreams = assertDoesNotThrow(() -> parseAndGetUpstreams(annotations));
        assertEquals(3, upstreams.size());
        assertEquals("triple://", upstreams.get(0).getProtocol());
        assertEquals("dubbo://", upstreams.get(1).getProtocol());
        assertEquals("dubbo://", upstreams.get(2).getProtocol());
    }

    @Test
    public void testEmptyProtocolAnnotation() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.UPSTREAMS_PROTOCOL_ANNOTATION_KEY, "");
        List<DubboUpstream> upstreams = assertDoesNotThrow(() -> parseAndGetUpstreams(annotations));
        assertNotNull(upstreams);
    }

    @Test
    public void shouldParseIngressWhenAnnotationsAreNull() {
        ShenyuMemoryConfig config = assertDoesNotThrow(() -> createParser().parse(
                createIngress(null, metadataLabels(), true), null));

        String handle = config.getRouteConfigList().get(0).getSelectorData().getHandle();
        List<DubboUpstream> upstreams = GsonUtils.getInstance().fromList(handle, DubboUpstream.class);
        assertEquals(1, upstreams.size());
        assertEquals("dubbo://", upstreams.get(0).getProtocol());
    }

    @Test
    public void shouldIgnorePathWithNullBackend() {
        ShenyuMemoryConfig config = Assertions.assertDoesNotThrow(() -> createParser().parse(
                createIngress(null, metadataLabels(), false), null));

        Assertions.assertEquals(1, config.getRouteConfigList().size());
        Assertions.assertEquals("[]", config.getRouteConfigList().get(0).getSelectorData().getHandle());
    }

    private DubboIngressParser createParser() {
        Indexer<V1Service> serviceIndexer = mock(Indexer.class);
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints endpoints = new V1EndpointsBuilder().withSubsets(new V1EndpointSubsetBuilder()
                .withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build()).build();
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(endpoints);
        return new DubboIngressParser(new Lister<>(serviceIndexer), new Lister<>(endpointsIndexer));
    }

    private Map<String, String> metadataLabels() {
        return Collections.singletonMap("shenyu.apache.org/metadata-labels-1", SERVICE_NAME);
    }

    private V1Ingress createIngress(final Map<String, String> annotations, final Map<String, String> labels,
                                    final boolean withBackend) {
        V1HTTPIngressPathBuilder pathBuilder = new V1HTTPIngressPathBuilder().withPath("/test").withPathType("Prefix");
        if (withBackend) {
            pathBuilder.withNewBackend().withNewService().withName(SERVICE_NAME).withNewPort().withNumber(8080)
                    .endPort().endService().endBackend();
        }
        return new V1IngressBuilder().withNewMetadata().withName("test-ingress").withNamespace(NAMESPACE)
                .withAnnotations(annotations).withLabels(labels).endMetadata()
                .withNewSpec().withRules(new V1IngressRuleBuilder().withNewHttp().withPaths(pathBuilder.build())
                        .endHttp().build()).endSpec().build();
    }

    private Lister<V1Service> newServiceLister() {
        final SharedIndexInformer<V1Service> informer = mock(SharedIndexInformer.class);
        final Indexer<V1Service> indexer = mock(Indexer.class);
        when(informer.getIndexer()).thenReturn(indexer);
        return new Lister<>(indexer);
    }

    private Lister<V1Endpoints> newEndpointsLister() {
        final SharedIndexInformer<V1Endpoints> informer = mock(SharedIndexInformer.class);
        final Indexer<V1Endpoints> indexer = mock(Indexer.class);
        final V1Endpoints endpoints = new V1EndpointsBuilder()
                .withNewMetadata().withName(SERVICE_NAME).withNamespace(NAMESPACE).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder()
                        .withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(indexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(endpoints);
        when(informer.getIndexer()).thenReturn(indexer);
        return new Lister<>(indexer);
    }

    @Test
    public void testParseWithNullLabels() {
        final Map<String, String> annotations = new HashMap<>();
        annotations.put("kubernetes.io/ingress.class", "shenyu");
        annotations.put("shenyu.apache.org/plugin-dubbo-enabled", "true");
        annotations.put("shenyu.apache.org/upstreams-protocol", "dubbo://,dubbo://");

        final V1IngressRule rule = new V1IngressRuleBuilder()
                .withNewHttp()
                .withPaths(new V1HTTPIngressPathBuilder()
                        .withPath("/dubbo/findById")
                        .withNewBackend()
                        .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(20888).endPort().endService()
                        .endBackend().build())
                .endHttp().build();

        final V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("testIngress").withNamespace(NAMESPACE)
                .withAnnotations(annotations).withLabels(null).endMetadata()
                .withNewSpec().withRules(rule).endSpec()
                .build();

        final DubboIngressParser parser = new DubboIngressParser(newServiceLister(), newEndpointsLister());
        final ShenyuMemoryConfig result = parser.parse(ingress, null);
        Assertions.assertNotNull(result);
    }

    @Test
    public void testParseWithMissingService() {
        final Map<String, String> annotations = new HashMap<>();
        annotations.put("kubernetes.io/ingress.class", "shenyu");
        annotations.put("shenyu.apache.org/plugin-dubbo-enabled", "true");
        annotations.put("shenyu.apache.org/upstreams-protocol", "dubbo://,dubbo://");

        final Map<String, String> labels = new HashMap<>();
        labels.put("shenyu.apache.org/metadata-labels-1", "nonExistentService");

        final V1IngressRule rule = new V1IngressRuleBuilder()
                .withNewHttp()
                .withPaths(new V1HTTPIngressPathBuilder()
                        .withPath("/dubbo/findById")
                        .withNewBackend()
                        .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(20888).endPort().endService()
                        .endBackend().build())
                .endHttp().build();

        final V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("testIngress").withNamespace(NAMESPACE)
                .withAnnotations(annotations).withLabels(labels).endMetadata()
                .withNewSpec().withRules(rule).endSpec()
                .build();

        final DubboIngressParser parser = new DubboIngressParser(newServiceLister(), newEndpointsLister());
        final ShenyuMemoryConfig result = parser.parse(ingress, null);
        Assertions.assertNotNull(result);
    }

    @Test
    public void testParseWithValidLabelsAndService() {
        final SharedIndexInformer<V1Service> serviceInformer = mock(SharedIndexInformer.class);
        final Indexer<V1Service> serviceIndexer = mock(Indexer.class);
        when(serviceInformer.getIndexer()).thenReturn(serviceIndexer);

        final Map<String, String> annotations = new HashMap<>();
        annotations.put("kubernetes.io/ingress.class", "shenyu");
        annotations.put("shenyu.apache.org/plugin-dubbo-enabled", "true");
        annotations.put("shenyu.apache.org/upstreams-protocol", "dubbo://,dubbo://");

        final Map<String, String> labels = new HashMap<>();
        labels.put("shenyu.apache.org/metadata-labels-1", "dubboFindIdService");

        final Map<String, String> serviceAnnotations = new HashMap<>();
        serviceAnnotations.put("shenyu.apache.org/plugin-dubbo-app-name", "dubbo");
        serviceAnnotations.put("shenyu.apache.org/plugin-dubbo-path", "/findById");
        serviceAnnotations.put("shenyu.apache.org/plugin-dubbo-rpc-type", "dubbo");
        serviceAnnotations.put("shenyu.apache.org/plugin-dubbo-service-name",
                "org.apache.shenyu.examples.dubbo.api.service.DubboTestService");
        serviceAnnotations.put("shenyu.apache.org/plugin-dubbo-method-name", "findById");
        serviceAnnotations.put("shenyu.apache.org/plugin-dubbo-params-type", "java.lang.String");

        final V1Service dubboService = new V1ServiceBuilder()
                .withNewMetadata().withName("dubboFindIdService").withNamespace(NAMESPACE)
                .withAnnotations(serviceAnnotations).endMetadata()
                .build();
        when(serviceIndexer.getByKey(NAMESPACE + "/dubboFindIdService")).thenReturn(dubboService);

        final V1IngressRule rule = new V1IngressRuleBuilder()
                .withNewHttp()
                .withPaths(new V1HTTPIngressPathBuilder()
                        .withPath("/dubbo/findById")
                        .withNewBackend()
                        .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(20888).endPort().endService()
                        .endBackend().build())
                .endHttp().build();

        final V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("testIngress").withNamespace(NAMESPACE)
                .withAnnotations(annotations).withLabels(labels).endMetadata()
                .withNewSpec().withRules(rule).endSpec()
                .build();

        final Lister<V1Service> serviceLister = new Lister<>(serviceIndexer);
        final Lister<V1Endpoints> endpointsLister = newEndpointsLister();
        final DubboIngressParser parser = new DubboIngressParser(serviceLister, endpointsLister);
        final ShenyuMemoryConfig result = parser.parse(ingress, null);
        Assertions.assertNotNull(result);
        Assertions.assertNotNull(result.getRouteConfigList());
        Assertions.assertFalse(result.getRouteConfigList().isEmpty());
    }
}
