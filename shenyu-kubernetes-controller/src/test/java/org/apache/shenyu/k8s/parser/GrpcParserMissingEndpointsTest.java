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
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.CoreV1EndpointPort;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubsetBuilder;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsBuilder;
import io.kubernetes.client.openapi.models.V1HTTPIngressPathBuilder;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBackend;
import io.kubernetes.client.openapi.models.V1IngressBackendBuilder;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1IngressSpec;
import io.kubernetes.client.openapi.models.V1IngressSpecBuilder;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.Objects;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test for parsing a grpc ingress whose backend service has no endpoints yet.
 */
public final class GrpcParserMissingEndpointsTest {

    private static final String NAMESPACE = "grpc-missing-endpoints-ns";

    private static final String SERVICE_NAME = "grpc-missing-endpoints-service";

    private static final String INGRESS_NAME = "grpc-missing-endpoints-ingress";

    private static final int SERVICE_PORT = 38080;

    private Indexer<V1Endpoints> endpointsIndexer;

    private GrpcParser grpcParser;

    @BeforeEach
    public void init() {
        endpointsIndexer = mock(Indexer.class);
        Indexer<V1Service> serviceIndexer = mock(Indexer.class);
        grpcParser = new GrpcParser(new Lister<>(serviceIndexer), new Lister<>(endpointsIndexer));
    }

    /**
     * test parse the ingress default backend when the endpoints of the service are absent.
     */
    @Test
    public void testParseDefaultBackendWithoutEndpoints() {
        V1Ingress ingress = buildIngress(backend(), null);

        ShenyuMemoryConfig config = Assertions.assertDoesNotThrow(
                () -> grpcParser.parse(ingress, mock(CoreV1Api.class)));

        Pair<Pair<String, String>, IngressConfiguration> globalDefaultBackend = config.getGlobalDefaultBackend();
        Assertions.assertNotNull(globalDefaultBackend);
        Assertions.assertEquals("[]", globalDefaultBackend.getRight().getSelectorData().getHandle());
    }

    /**
     * test parse the ingress rule backend when the endpoints of the service are absent.
     */
    @Test
    public void testParseRuleBackendWithoutEndpoints() {
        V1Ingress ingress = buildIngress(null, rule(backend()));

        ShenyuMemoryConfig config = Assertions.assertDoesNotThrow(
                () -> grpcParser.parse(ingress, mock(CoreV1Api.class)));

        Assertions.assertEquals(1, config.getRouteConfigList().size());
        Assertions.assertEquals("[]", config.getRouteConfigList().get(0).getSelectorData().getHandle());
    }

    /**
     * test parse the ingress rule backend when the endpoints of the service are present.
     */
    @Test
    public void testParseRuleBackendWithEndpoints() {
        mockEndpoints();
        V1Ingress ingress = buildIngress(null, rule(backend()));

        ShenyuMemoryConfig config = grpcParser.parse(ingress, mock(CoreV1Api.class));

        Assertions.assertEquals(1, config.getRouteConfigList().size());
        assertThat(config.getRouteConfigList().get(0).getSelectorData().getHandle(),
                containsString("127.0.0.1:" + SERVICE_PORT));
    }

    private void mockEndpoints() {
        V1Endpoints endpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace(NAMESPACE).withName(SERVICE_NAME).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder()
                        .withAddresses(new V1EndpointAddress().ip("127.0.0.1"))
                        .withPorts(new CoreV1EndpointPort().port(SERVICE_PORT).protocol("TCP"))
                        .build())
                .build();
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(endpoints);
    }

    private V1IngressBackend backend() {
        return new V1IngressBackendBuilder()
                .withNewService().withName(SERVICE_NAME).withNewPort().withNumber(SERVICE_PORT).endPort().endService()
                .build();
    }

    private V1IngressRule rule(final V1IngressBackend backend) {
        return new V1IngressRuleBuilder().withNewHttp().withPaths(
                        new V1HTTPIngressPathBuilder().withPath("/grpc/").withPathType("Prefix").withBackend(backend).build())
                .endHttp().build();
    }

    private V1Ingress buildIngress(final V1IngressBackend defaultBackend, final V1IngressRule rule) {
        V1IngressSpec spec = new V1IngressSpecBuilder()
                .withDefaultBackend(defaultBackend)
                .withRules(Objects.isNull(rule) ? null : Collections.singletonList(rule))
                .build();
        return new V1IngressBuilder().withNewMetadata()
                        .withName(INGRESS_NAME).withNamespace(NAMESPACE)
                        .withAnnotations(Collections.emptyMap()).withLabels(Collections.emptyMap()).endMetadata()
                .withSpec(spec)
                .withKind("Ingress").build();
    }
}
