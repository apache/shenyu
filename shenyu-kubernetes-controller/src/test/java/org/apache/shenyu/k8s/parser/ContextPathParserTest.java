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
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1Service;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1HTTPIngressPathBuilder;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.apache.shenyu.common.dto.RuleData;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Unit tests for {@link ContextPathParser}.
 */
public final class ContextPathParserTest {

    private SharedIndexInformer<V1Service> serviceInformer;

    private SharedIndexInformer<V1Endpoints> endpointsInformer;

    private ContextPathParser contextPathParser;

    private CoreV1Api coreV1Api;

    @BeforeEach
    public void setUp() {
        serviceInformer = mock(SharedIndexInformer.class);
        endpointsInformer = mock(SharedIndexInformer.class);

        Lister<V1Service> serviceLister = mock(Lister.class);
        when(serviceInformer.getIndexer()).thenReturn(mock(io.kubernetes.client.informer.cache.Indexer.class));

        Lister<V1Endpoints> endpointsLister = mock(Lister.class);
        when(endpointsInformer.getIndexer()).thenReturn(mock(io.kubernetes.client.informer.cache.Indexer.class));

        contextPathParser = new ContextPathParser(serviceLister, endpointsLister);
        coreV1Api = mock(CoreV1Api.class);
    }

    /**
     * Test that parsing an ingress without the context-path annotation
     * does NOT produce a rule with "null/**" as the path pattern.
     * This is the regression test for GitHub issue #6863.
     */
    @Test
    public void testParseWithoutContextPathAnnotation() {
        // Build ingress WITHOUT the context-path annotation
        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata()
                .withName("test-ingress")
                .withNamespace("default")
                .withAnnotations(Map.of("kubernetes.io/ingress.class", "shenyu"))
                .endMetadata()
                .withNewSpec()
                .withRules(
                        new V1IngressRuleBuilder()
                                .withNewHttp()
                                .withPaths(
                                        new V1HTTPIngressPathBuilder()
                                                .withPath("/api")
                                                .withPathType("Prefix")
                                                .withNewBackend()
                                                .withNewService()
                                                .withName("test-service")
                                                .withNewPort()
                                                .withNumber(8080)
                                                .endPort()
                                                .endService()
                                                .endBackend()
                                                .build()
                                )
                                .endHttp()
                                .build()
                )
                .endSpec()
                .build();

        ShenyuMemoryConfig result = contextPathParser.parse(ingress, coreV1Api);
        List<IngressConfiguration> routeConfigs = result.getRouteConfigList();

        Assertions.assertNotNull(routeConfigs);
        Assertions.assertEquals(1, routeConfigs.size());

        IngressConfiguration routeConfig = routeConfigs.get(0);
        List<RuleData> ruleDataList = routeConfig.getRuleDataList();

        // The critical assertion: ruleDataList should be empty when
        // no context-path annotation is present. Before the fix, it would
        // contain a rule with paramValue "null/**" which never matches real traffic.
        Assertions.assertTrue(ruleDataList.isEmpty(),
                "Rule list should be empty when context-path annotation is absent, "
                + "but got rules: " + ruleDataList);
    }

    /**
     * Test that parsing an ingress WITH the context-path annotation
     * produces a valid rule with the correct path pattern.
     */
    @Test
    public void testParseWithContextPathAnnotation() {
        String contextPath = "/myapp";
        String addPrefix = "/myapp";

        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.PLUGIN_CONTEXT_PATH_PATH, contextPath);
        annotations.put(IngressConstants.PLUGIN_CONTEXT_PATH_ADD_PREFIX, addPrefix);
        annotations.put(IngressConstants.PLUGIN_CONTEXT_PATH_ADD_PREFIXED, "true");

        // Build ingress WITH the context-path annotation
        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata()
                .withName("test-ingress-with-annotation")
                .withNamespace("default")
                .withAnnotations(annotations)
                .endMetadata()
                .withNewSpec()
                .withRules(
                        new V1IngressRuleBuilder()
                                .withNewHttp()
                                .withPaths(
                                        new V1HTTPIngressPathBuilder()
                                                .withPath("/api")
                                                .withPathType("Prefix")
                                                .withNewBackend()
                                                .withNewService()
                                                .withName("test-service")
                                                .withNewPort()
                                                .withNumber(8080)
                                                .endPort()
                                                .endService()
                                                .endBackend()
                                                .build()
                                )
                                .endHttp()
                                .build()
                )
                .endSpec()
                .build();

        ShenyuMemoryConfig result = contextPathParser.parse(ingress, coreV1Api);
        List<IngressConfiguration> routeConfigs = result.getRouteConfigList();

        Assertions.assertNotNull(routeConfigs);
        Assertions.assertEquals(1, routeConfigs.size());

        IngressConfiguration routeConfig = routeConfigs.get(0);
        List<RuleData> ruleDataList = routeConfig.getRuleDataList();

        // With the annotation, a rule should be present
        Assertions.assertFalse(ruleDataList.isEmpty(),
                "Rule list should not be empty when context-path annotation is present");
        Assertions.assertEquals(1, ruleDataList.size());

        RuleData ruleData = ruleDataList.get(0);
        Assertions.assertEquals(contextPath, ruleData.getName());

        // Verify the path pattern is correct, NOT "null/**"
        org.apache.shenyu.common.dto.ConditionData condition = ruleData.getConditionDataList().get(0);
        Assertions.assertEquals(contextPath + "/**", condition.getParamValue(),
                "Path pattern should be the contextPath + /**, not 'null/**'");
    }

    /**
     * Test that parsing an ingress with empty-string annotation value
     * is treated as non-null and produces a rule.
     */
    @Test
    public void testParseWithEmptyStringContextPathAnnotation() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.PLUGIN_CONTEXT_PATH_PATH, "");

        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata()
                .withName("test-ingress-empty")
                .withNamespace("default")
                .withAnnotations(annotations)
                .endMetadata()
                .withNewSpec()
                .withRules(
                        new V1IngressRuleBuilder()
                                .withNewHttp()
                                .withPaths(
                                        new V1HTTPIngressPathBuilder()
                                                .withPath("/api")
                                                .withPathType("Prefix")
                                                .withNewBackend()
                                                .withNewService()
                                                .withName("test-service")
                                                .withNewPort()
                                                .withNumber(8080)
                                                .endPort()
                                                .endService()
                                                .endBackend()
                                                .build()
                                )
                                .endHttp()
                                .build()
                )
                .endSpec()
                .build();

        ShenyuMemoryConfig result = contextPathParser.parse(ingress, coreV1Api);
        List<IngressConfiguration> routeConfigs = result.getRouteConfigList();

        Assertions.assertNotNull(routeConfigs);
        Assertions.assertEquals(1, routeConfigs.size());

        IngressConfiguration routeConfig = routeConfigs.get(0);
        List<RuleData> ruleDataList = routeConfig.getRuleDataList();

        // Empty string is still a valid value (not null), so a rule is created
        // with paramValue "/**" (empty + "/**").
        Assertions.assertFalse(ruleDataList.isEmpty());
        Assertions.assertEquals(1, ruleDataList.size());
        Assertions.assertEquals("", ruleDataList.get(0).getName());
        Assertions.assertEquals("/**", ruleDataList.get(0).getConditionDataList().get(0).getParamValue());
    }
}
