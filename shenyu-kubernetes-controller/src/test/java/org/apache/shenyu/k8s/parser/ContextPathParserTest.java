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
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1HTTPIngressPath;
import io.kubernetes.client.openapi.models.V1HTTPIngressPathBuilder;
import io.kubernetes.client.openapi.models.V1Ingress;
import io.kubernetes.client.openapi.models.V1IngressBuilder;
import io.kubernetes.client.openapi.models.V1IngressRule;
import io.kubernetes.client.openapi.models.V1IngressRuleBuilder;
import io.kubernetes.client.openapi.models.V1Service;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.ContextMappingRuleHandle;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.IngressConstants;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * Test for {@link ContextPathParser}.
 */
public class ContextPathParserTest {

    private static final String NAMESPACE = "context-path-namespace";

    private Lister<V1Service> serviceLister;

    private Lister<V1Endpoints> endpointsLister;

    @BeforeEach
    @SuppressWarnings("unchecked")
    public void setUp() {
        serviceLister = new Lister<>(mock(Indexer.class));
        endpointsLister = new Lister<>(mock(Indexer.class));
    }

    @Test
    public void testParseContextPathSelectorAndRule() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.PLUGIN_CONTEXT_PATH_PATH, "/api");
        annotations.put(IngressConstants.PLUGIN_CONTEXT_PATH_ADD_PREFIX, "/prefix");
        annotations.put(IngressConstants.PLUGIN_CONTEXT_PATH_ADD_PREFIXED, "true");

        ShenyuMemoryConfig config = parse(createIngress(annotations, createRule("www.example.com",
                createPath("/context", "Prefix"))));

        List<IngressConfiguration> routeConfigList = config.getRouteConfigList();
        assertNotNull(routeConfigList);
        assertEquals(1, routeConfigList.size());

        SelectorData selectorData = routeConfigList.get(0).getSelectorData();
        assertEquals(PluginEnum.CONTEXT_PATH.getName(), selectorData.getPluginName());
        assertEquals(String.valueOf(PluginEnum.CONTEXT_PATH.getCode()), selectorData.getPluginId());
        assertEquals("/context", selectorData.getName());
        assertEquals(MatchModeEnum.AND.getCode(), selectorData.getMatchMode());
        assertEquals(2, selectorData.getConditionList().size());
        assertEquals(ParamTypeEnum.DOMAIN.getName(), selectorData.getConditionList().get(0).getParamType());
        assertEquals(OperatorEnum.EQ.getAlias(), selectorData.getConditionList().get(0).getOperator());
        assertEquals("www.example.com", selectorData.getConditionList().get(0).getParamValue());
        assertEquals(ParamTypeEnum.URI.getName(), selectorData.getConditionList().get(1).getParamType());
        assertEquals(OperatorEnum.STARTS_WITH.getAlias(), selectorData.getConditionList().get(1).getOperator());
        assertEquals("/context", selectorData.getConditionList().get(1).getParamValue());

        List<RuleData> ruleDataList = routeConfigList.get(0).getRuleDataList();
        assertEquals(1, ruleDataList.size());
        RuleData ruleData = ruleDataList.get(0);
        assertEquals("/api", ruleData.getName());
        assertEquals(PluginEnum.CONTEXT_PATH.getName(), ruleData.getPluginName());
        assertEquals(MatchModeEnum.AND.getCode(), ruleData.getMatchMode());
        assertEquals(1, ruleData.getConditionDataList().size());
        assertEquals(OperatorEnum.PATH_PATTERN.getAlias(), ruleData.getConditionDataList().get(0).getOperator());
        assertEquals("/api/**", ruleData.getConditionDataList().get(0).getParamValue());

        ContextMappingRuleHandle ruleHandle = GsonUtils.getInstance().fromJson(ruleData.getHandle(), ContextMappingRuleHandle.class);
        assertEquals("/api", ruleHandle.getContextPath());
        assertEquals("/prefix", ruleHandle.getAddPrefix());
        assertTrue(ruleHandle.getAddPrefixed());
    }

    @Test
    public void testParseRuleHandleDefaults() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.PLUGIN_CONTEXT_PATH_PATH, "/api");

        ShenyuMemoryConfig config = parse(createIngress(annotations, createRule(null, createPath("/context", "Prefix"))));

        List<RuleData> ruleDataList = config.getRouteConfigList().get(0).getRuleDataList();
        ContextMappingRuleHandle ruleHandle = GsonUtils.getInstance().fromJson(ruleDataList.get(0).getHandle(), ContextMappingRuleHandle.class);
        assertEquals("/api", ruleHandle.getContextPath());
        assertNull(ruleHandle.getAddPrefix());
        assertFalse(ruleHandle.getAddPrefixed());
    }

    @Test
    public void testParsePathTypeToOperatorMapping() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.PLUGIN_CONTEXT_PATH_PATH, "/api");

        assertEquals(OperatorEnum.EQ.getAlias(), parsePathOperator(annotations, "Exact"));
        assertEquals(OperatorEnum.STARTS_WITH.getAlias(), parsePathOperator(annotations, "Prefix"));
        assertEquals(OperatorEnum.MATCH.getAlias(), parsePathOperator(annotations, "ImplementationSpecific"));
        assertEquals(OperatorEnum.MATCH.getAlias(), parsePathOperator(annotations, "Unknown"));
    }

    @Test
    public void testParseIngressRuleWithNullPath() {
        Map<String, String> annotations = new HashMap<>();
        annotations.put(IngressConstants.PLUGIN_CONTEXT_PATH_PATH, "/api");

        ShenyuMemoryConfig config = parse(createIngress(annotations, createRule(null, createPath(null, "Prefix"))));

        List<IngressConfiguration> routeConfigList = config.getRouteConfigList();
        assertNotNull(routeConfigList);
        assertEquals(0, routeConfigList.size());
    }

    @Test
    public void testParseIngressWithoutRules() {
        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("context-path-ingress").withNamespace(NAMESPACE)
                .withAnnotations(new HashMap<>()).endMetadata()
                .withNewSpec().endSpec()
                .build();

        ShenyuMemoryConfig config = parse(ingress);

        assertNull(config.getRouteConfigList());
    }

    @Test
    public void testParseIngressWithoutSpec() {
        V1Ingress ingress = new V1IngressBuilder()
                .withNewMetadata().withName("context-path-ingress").withNamespace(NAMESPACE)
                .withAnnotations(new HashMap<>()).endMetadata()
                .build();

        ShenyuMemoryConfig config = parse(ingress);

        assertNull(config.getRouteConfigList());
    }

    private String parsePathOperator(final Map<String, String> annotations, final String pathType) {
        ShenyuMemoryConfig config = parse(createIngress(annotations, createRule(null, createPath("/context", pathType))));
        return config.getRouteConfigList().get(0).getSelectorData().getConditionList().get(0).getOperator();
    }

    private ShenyuMemoryConfig parse(final V1Ingress ingress) {
        return new ContextPathParser(serviceLister, endpointsLister).parse(ingress, null);
    }

    private V1Ingress createIngress(final Map<String, String> annotations, final V1IngressRule rule) {
        return new V1IngressBuilder()
                .withNewMetadata().withName("context-path-ingress").withNamespace(NAMESPACE).withAnnotations(annotations).endMetadata()
                .withNewSpec().withRules(rule).endSpec()
                .withKind("Ingress")
                .build();
    }

    private V1IngressRule createRule(final String host, final V1HTTPIngressPath path) {
        return new V1IngressRuleBuilder().withHost(host).withNewHttp().withPaths(path).endHttp().build();
    }

    private V1HTTPIngressPath createPath(final String path, final String pathType) {
        return new V1HTTPIngressPathBuilder().withPath(path).withPathType(pathType).build();
    }
}
