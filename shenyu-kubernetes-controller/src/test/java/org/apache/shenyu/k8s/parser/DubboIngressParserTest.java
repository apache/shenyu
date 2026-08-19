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

import io.kubernetes.client.informer.cache.Indexer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubsetBuilder;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsBuilder;
import io.kubernetes.client.openapi.models.V1HTTPIngressPathBuilder;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.shenyu.common.dto.convert.selector.DubboUpstream;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

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
        Map<String, String> labels = new HashMap<>();

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
}
