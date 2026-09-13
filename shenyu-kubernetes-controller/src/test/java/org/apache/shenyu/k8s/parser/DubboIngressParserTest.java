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
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Tests for null-safe Dubbo ingress upstream parsing.
 */
public final class DubboIngressParserTest {

    private static final String NAMESPACE = "test-namespace";

    private static final String SERVICE_NAME = "backend-service";

    @Test
    public void shouldIgnorePathWithNullBackend() {
        ShenyuMemoryConfig config = Assertions.assertDoesNotThrow(() -> createParser().parse(
                createIngress(null, Collections.emptyMap(), false), null));

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
}
