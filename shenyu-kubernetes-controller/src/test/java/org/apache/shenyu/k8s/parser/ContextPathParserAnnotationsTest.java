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
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1HTTPIngressPathBuilder;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1Service;
import io.kubernetes.client.openapi.models.V1ServiceBuilder;
import org.apache.shenyu.common.dto.convert.rule.impl.ContextMappingRuleHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test for resolving the context path annotations of an ingress.
 */
public final class ContextPathParserAnnotationsTest {

    private static final String NAMESPACE = "context-path-ns";

    private static final String METADATA_SERVICE = "context-path-metadata-service";

    private static final String CONTEXT_PATH = "/grpc";

    private Indexer<V1Service> serviceIndexer;

    private ContextPathParser contextPathParser;

    @BeforeEach
    public void init() {
        serviceIndexer = mock(Indexer.class);
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        contextPathParser = new ContextPathParser(new Lister<>(serviceIndexer), new Lister<>(endpointsIndexer));
    }

    /**
     * test parse a Prefix path ingress that does not declare the context path annotation.
     */
    @Test
    public void testParseWithoutContextPathAnnotation() {
        ShenyuMemoryConfig config = contextPathParser.parse(buildIngress(null, null), mock(CoreV1Api.class));

        Assertions.assertNotNull(config.getRouteConfigList());
        Assertions.assertTrue(config.getRouteConfigList().isEmpty());
    }

    /**
     * test parse the context path annotation from the metadata service of the ingress labels.
     */
    @Test
    public void testParseContextPathFromMetadataService() {
        mockMetadataService(CONTEXT_PATH);
        Map<String, String> labels = Collections.singletonMap("shenyu.apache.org/metadata-labels-1", METADATA_SERVICE);

        ShenyuMemoryConfig config = contextPathParser.parse(buildIngress(null, labels), mock(CoreV1Api.class));

        List<IngressConfiguration> routeConfigList = config.getRouteConfigList();
        Assertions.assertEquals(1, routeConfigList.size());
        ContextMappingRuleHandle ruleHandle = GsonUtils.getInstance().fromJson(
                routeConfigList.get(0).getRuleDataList().get(0).getHandle(), ContextMappingRuleHandle.class);
        Assertions.assertEquals(CONTEXT_PATH, ruleHandle.getContextPath());
        Assertions.assertEquals(CONTEXT_PATH, routeConfigList.get(0).getRuleDataList().get(0).getName());
        Assertions.assertEquals(CONTEXT_PATH + "/**",
                routeConfigList.get(0).getRuleDataList().get(0).getConditionDataList().get(0).getParamValue());
    }

    /**
     * test parse an ingress that declares the context path annotation itself.
     */
    @Test
    public void testIngressAnnotationWins() {
        mockMetadataService("/sofa");
        Map<String, String> annotations = Collections.singletonMap(IngressConstants.PLUGIN_CONTEXT_PATH_PATH, CONTEXT_PATH);
        Map<String, String> labels = Collections.singletonMap("shenyu.apache.org/metadata-labels-1", METADATA_SERVICE);

        ShenyuMemoryConfig config = contextPathParser.parse(buildIngress(annotations, labels), mock(CoreV1Api.class));

        List<IngressConfiguration> routeConfigList = config.getRouteConfigList();
        Assertions.assertEquals(1, routeConfigList.size());
        ContextMappingRuleHandle ruleHandle = GsonUtils.getInstance().fromJson(
                routeConfigList.get(0).getRuleDataList().get(0).getHandle(), ContextMappingRuleHandle.class);
        Assertions.assertEquals(CONTEXT_PATH, ruleHandle.getContextPath());
    }

    /**
     * test parse an ingress that references labels which are not registered services.
     */
    @Test
    public void testParseWithUnknownLabelServices() {
        Map<String, String> labels = new HashMap<>();
        labels.put("app", "shenyu-examples-grpc-service");
        labels.put("shenyu.apache.org/metadata-labels-1", METADATA_SERVICE);

        ShenyuMemoryConfig config = contextPathParser.parse(buildIngress(null, labels), mock(CoreV1Api.class));

        Assertions.assertNotNull(config.getRouteConfigList());
        Assertions.assertTrue(config.getRouteConfigList().isEmpty());
    }

    private void mockMetadataService(final String contextPath) {
        V1Service service = new V1ServiceBuilder().withNewMetadata()
                        .withName(METADATA_SERVICE).withNamespace(NAMESPACE)
                        .withAnnotations(Collections.singletonMap(IngressConstants.PLUGIN_CONTEXT_PATH_PATH, contextPath)).endMetadata()
                .withNewSpec().endSpec()
                .withKind("Service").build();
        when(serviceIndexer.getByKey(NAMESPACE + "/" + METADATA_SERVICE)).thenReturn(service);
    }

    private V1Ingress buildIngress(final Map<String, String> annotations, final Map<String, String> labels) {
        V1IngressRule rule = new V1IngressRuleBuilder().withNewHttp().withPaths(
                        new V1HTTPIngressPathBuilder().withPath("/grpc/").withPathType("Prefix")
                                .withNewBackend()
                                    .withNewService().withName("shenyu-examples-grpc-service").withNewPort().withNumber(38080).endPort().endService()
                                .endBackend().build())
                .endHttp().build();
        return new V1IngressBuilder().withNewMetadata()
                        .withName("context-path-ingress").withNamespace(NAMESPACE)
                        .withAnnotations(annotations).withLabels(labels).endMetadata()
                .withNewSpec().withRules(rule).endSpec()
                .withKind("Ingress").build();
    }
}
