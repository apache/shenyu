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

package org.apache.shenyu.k8s;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import io.kubernetes.client.informer.cache.Indexer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubsetBuilder;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsBuilder;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.k8s.common.IngressConfiguration;
import org.apache.shenyu.k8s.common.ShenyuMemoryConfig;
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Objects;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * HttpRouteParser Test.
 */
public final class HttpRouteParserTest {

    private static final String NAMESPACE = "test-ns";

    private static final String SERVICE_NAME = "test-service";

    private static final int SERVICE_PORT = 8189;

    /**
     * Test parse with path prefix match.
     */
    @Test
    public void testParseWithPathPrefix() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister);

        DynamicKubernetesObject httpRoute = buildHTTPRoute(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/api/**", "PathPrefix", null, null);
        ShenyuMemoryConfig config = parser.parse(httpRoute);

        List<SelectorData> selectors = extractSelectors(config);
        List<RuleData> rules = extractRules(config);
        Assertions.assertEquals(1, selectors.size());
        Assertions.assertEquals(1, rules.size());

        ConditionData pathCondition = config.getRouteConfigList().get(0).getSelectorData().getConditionList().get(0);
        Assertions.assertEquals(ParamTypeEnum.URI.getName(), pathCondition.getParamType());
        Assertions.assertEquals(OperatorEnum.STARTS_WITH.getAlias(), pathCondition.getOperator());
        Assertions.assertEquals("/api/**", pathCondition.getParamValue());
    }

    /**
     * Test parse with exact path match.
     */
    @Test
    public void testParseWithExactPath() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister);

        DynamicKubernetesObject httpRoute = buildHTTPRoute(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/api/v1/test", "Exact", null, null);
        ShenyuMemoryConfig config = parser.parse(httpRoute);

        ConditionData pathCondition = config.getRouteConfigList().get(0).getSelectorData().getConditionList().get(0);
        Assertions.assertEquals(OperatorEnum.EQ.getAlias(), pathCondition.getOperator());
    }

    /**
     * Test parse with regex path match.
     */
    @Test
    public void testParseWithRegexPath() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister);

        DynamicKubernetesObject httpRoute = buildHTTPRoute(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/api/v[0-9]+/.*", "RegularExpression", null, null);
        ShenyuMemoryConfig config = parser.parse(httpRoute);

        ConditionData pathCondition = config.getRouteConfigList().get(0).getSelectorData().getConditionList().get(0);
        Assertions.assertEquals(OperatorEnum.MATCH.getAlias(), pathCondition.getOperator());
    }

    /**
     * Test parse with hostname conditions.
     * Each hostname should generate a separate selector+rule (one hostname per selector).
     */
    @Test
    public void testParseWithHostnames() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister);

        DynamicKubernetesObject httpRoute = buildHTTPRouteWithHostnames(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/**", "PathPrefix", new String[]{"example.com", "api.example.com"});
        ShenyuMemoryConfig config = parser.parse(httpRoute);

        // Should have 2 selectors (one per hostname), each with 1 host + 1 path condition
        List<IngressConfiguration> routeConfigs = config.getRouteConfigList();
        Assertions.assertEquals(2, routeConfigs.size());

        for (IngressConfiguration routeConfig : routeConfigs) {
            List<ConditionData> conditions = routeConfig.getSelectorData().getConditionList();
            long hostConditions = conditions.stream()
                    .filter(c -> ParamTypeEnum.DOMAIN.getName().equals(c.getParamType()))
                    .count();
            Assertions.assertEquals(1, hostConditions);

            long pathConditions = conditions.stream()
                    .filter(c -> ParamTypeEnum.URI.getName().equals(c.getParamType()))
                    .count();
            Assertions.assertEquals(1, pathConditions);
        }
    }

    /**
     * Test parse with header match.
     */
    @Test
    public void testParseWithHeaderMatch() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister);

        DynamicKubernetesObject httpRoute = buildHTTPRoute(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/**", "PathPrefix", "X-Custom-Header", "test-value");
        ShenyuMemoryConfig config = parser.parse(httpRoute);

        List<ConditionData> conditions = config.getRouteConfigList().get(0).getSelectorData().getConditionList();
        long headerConditions = conditions.stream()
                .filter(c -> ParamTypeEnum.HEADER.getName().equals(c.getParamType()))
                .count();
        Assertions.assertEquals(1, headerConditions);

        ConditionData headerCondition = conditions.stream()
                .filter(c -> ParamTypeEnum.HEADER.getName().equals(c.getParamType()))
                .findFirst().orElse(null);
        Assertions.assertNotNull(headerCondition);
        Assertions.assertEquals("X-Custom-Header", headerCondition.getParamName());
        Assertions.assertEquals("test-value", headerCondition.getParamValue());
        Assertions.assertEquals(OperatorEnum.EQ.getAlias(), headerCondition.getOperator());
    }

    /**
     * Test parse with no rules: should return empty config.
     */
    @Test
    public void testParseWithNoRules() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister);

        DynamicKubernetesObject httpRoute = buildHTTPRouteNoRules(NAMESPACE, "empty-route");
        ShenyuMemoryConfig config = parser.parse(httpRoute);

        Assertions.assertTrue(Objects.isNull(config.getRouteConfigList()) || config.getRouteConfigList().isEmpty());
    }

    /**
     * Test parse with no matching endpoints: should return empty upstream list but still create selector/rule.
     */
    @Test
    public void testParseWithNoEndpoints() {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(null);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);

        HttpRouteParser parser = new HttpRouteParser(endpointsLister);

        DynamicKubernetesObject httpRoute = buildHTTPRoute(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/**", "PathPrefix", null, null);
        ShenyuMemoryConfig config = parser.parse(httpRoute);

        // Selector should still exist but with empty upstream handle
        List<SelectorData> selectors = extractSelectors(config);
        Assertions.assertEquals(1, selectors.size());
    }

    /**
     * Test parse with query param match.
     */
    @Test
    public void testParseWithQueryParam() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister);

        DynamicKubernetesObject httpRoute = buildHTTPRouteWithQueryParams(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/**", "PathPrefix", "debug", "true", "Exact");
        ShenyuMemoryConfig config = parser.parse(httpRoute);

        List<ConditionData> conditions = config.getRouteConfigList().get(0).getSelectorData().getConditionList();
        long queryConditions = conditions.stream()
                .filter(c -> ParamTypeEnum.QUERY.getName().equals(c.getParamType()))
                .count();
        Assertions.assertEquals(1, queryConditions);

        ConditionData queryCondition = conditions.stream()
                .filter(c -> ParamTypeEnum.QUERY.getName().equals(c.getParamType()))
                .findFirst().orElse(null);
        Assertions.assertNotNull(queryCondition);
        Assertions.assertEquals("debug", queryCondition.getParamName());
        Assertions.assertEquals("true", queryCondition.getParamValue());
    }

    /**
     * Test parse with multiple backend refs.
     */
    @Test
    public void testParseWithMultipleBackendRefs() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister);

        DynamicKubernetesObject httpRoute = buildHTTPRouteMultiBackend(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT, 8080);
        ShenyuMemoryConfig config = parser.parse(httpRoute);

        // Should create selector with multiple upstreams
        List<SelectorData> selectors = extractSelectors(config);
        Assertions.assertEquals(1, selectors.size());
    }

    private Lister<V1Endpoints> mockEndpointsLister() {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace(NAMESPACE).withName(SERVICE_NAME).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder()
                        .withAddresses(new V1EndpointAddress().ip("10.0.0.1"), new V1EndpointAddress().ip("10.0.0.2"))
                        .build())
                .build();
        when(endpointsIndexer.getByKey(NAMESPACE + "/" + SERVICE_NAME)).thenReturn(mockedEndpoints);
        return new Lister<>(endpointsIndexer);
    }

    private List<SelectorData> extractSelectors(final ShenyuMemoryConfig config) {
        return config.getRouteConfigList().stream().map(r -> r.getSelectorData()).toList();
    }

    private List<RuleData> extractRules(final ShenyuMemoryConfig config) {
        return config.getRouteConfigList().stream().flatMap(r -> r.getRuleDataList().stream()).toList();
    }

    private DynamicKubernetesObject buildHTTPRoute(final String routeNamespace, final String routeName,
                                                    final String gatewayNamespace, final String gatewayName,
                                                    final String serviceName, final int port,
                                                    final String pathValue, final String pathType,
                                                    final String headerName, final String headerValue) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", routeNamespace);
        metadata.addProperty("name", routeName);

        JsonObject parentRef = new JsonObject();
        parentRef.addProperty("name", gatewayName);
        parentRef.addProperty("namespace", gatewayNamespace);
        JsonArray parentRefs = new JsonArray();
        parentRefs.add(parentRef);

        JsonObject backendRef = new JsonObject();
        backendRef.addProperty("name", serviceName);
        backendRef.addProperty("port", port);
        JsonArray backendRefs = new JsonArray();
        backendRefs.add(backendRef);

        JsonObject pathMatch = new JsonObject();
        pathMatch.addProperty("type", pathType);
        pathMatch.addProperty("value", pathValue);
        JsonObject match = new JsonObject();
        match.add("path", pathMatch);

        if (Objects.nonNull(headerName) && Objects.nonNull(headerValue)) {
            JsonObject header = new JsonObject();
            header.addProperty("name", headerName);
            header.addProperty("value", headerValue);
            header.addProperty("type", "Exact");
            JsonArray headers = new JsonArray();
            headers.add(header);
            match.add("headers", headers);
        }

        JsonArray matches = new JsonArray();
        matches.add(match);

        JsonObject rule = new JsonObject();
        rule.add("backendRefs", backendRefs);
        rule.add("matches", matches);
        JsonArray rules = new JsonArray();
        rules.add(rule);

        JsonObject spec = new JsonObject();
        spec.add("parentRefs", parentRefs);
        spec.add("rules", rules);

        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "HTTPRoute");
        raw.add("metadata", metadata);
        raw.add("spec", spec);
        return new DynamicKubernetesObject(raw);
    }

    private DynamicKubernetesObject buildHTTPRouteWithHostnames(final String routeNamespace, final String routeName,
                                                                 final String gatewayNamespace, final String gatewayName,
                                                                 final String serviceName, final int port,
                                                                 final String pathValue, final String pathType,
                                                                 final String[] hostnames) {
        DynamicKubernetesObject httpRoute = buildHTTPRoute(routeNamespace, routeName,
                gatewayNamespace, gatewayName, serviceName, port, pathValue, pathType, null, null);

        JsonArray hostnameArray = new JsonArray();
        for (String hostname : hostnames) {
            hostnameArray.add(hostname);
        }
        httpRoute.getRaw().getAsJsonObject("spec").add("hostnames", hostnameArray);
        return httpRoute;
    }

    private DynamicKubernetesObject buildHTTPRouteWithQueryParams(final String routeNamespace, final String routeName,
                                                                   final String gatewayNamespace, final String gatewayName,
                                                                   final String serviceName, final int port,
                                                                   final String pathValue, final String pathType,
                                                                   final String queryName, final String queryValue,
                                                                   final String queryType) {
        // Add query params to the match
        JsonObject queryParam = new JsonObject();
        queryParam.addProperty("name", queryName);
        queryParam.addProperty("value", queryValue);
        queryParam.addProperty("type", queryType);
        JsonArray queryParams = new JsonArray();
        queryParams.add(queryParam);

        final DynamicKubernetesObject httpRoute = buildHTTPRoute(routeNamespace, routeName,
                gatewayNamespace, gatewayName, serviceName, port, pathValue, pathType, null, null);
        JsonArray rules = httpRoute.getRaw().getAsJsonObject("spec").getAsJsonArray("rules");
        JsonObject firstRule = rules.get(0).getAsJsonObject();
        JsonObject firstMatch = firstRule.getAsJsonArray("matches").get(0).getAsJsonObject();
        firstMatch.add("queryParams", queryParams);

        return httpRoute;
    }

    private DynamicKubernetesObject buildHTTPRouteMultiBackend(final String routeNamespace, final String routeName,
                                                                final String gatewayNamespace, final String gatewayName,
                                                                final String serviceName, final int port1, final int port2) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", routeNamespace);
        metadata.addProperty("name", routeName);

        JsonObject parentRef = new JsonObject();
        parentRef.addProperty("name", gatewayName);
        parentRef.addProperty("namespace", gatewayNamespace);
        JsonArray parentRefs = new JsonArray();
        parentRefs.add(parentRef);

        JsonObject backendRef1 = new JsonObject();
        backendRef1.addProperty("name", serviceName);
        backendRef1.addProperty("port", port1);
        backendRef1.addProperty("weight", 70);

        JsonObject backendRef2 = new JsonObject();
        backendRef2.addProperty("name", serviceName);
        backendRef2.addProperty("port", port2);
        backendRef2.addProperty("weight", 30);

        JsonArray backendRefs = new JsonArray();
        backendRefs.add(backendRef1);
        backendRefs.add(backendRef2);

        JsonObject pathMatch = new JsonObject();
        pathMatch.addProperty("type", "PathPrefix");
        pathMatch.addProperty("value", "/**");
        JsonObject match = new JsonObject();
        match.add("path", pathMatch);
        JsonArray matches = new JsonArray();
        matches.add(match);

        JsonObject rule = new JsonObject();
        rule.add("backendRefs", backendRefs);
        rule.add("matches", matches);
        JsonArray rules = new JsonArray();
        rules.add(rule);

        JsonObject spec = new JsonObject();
        spec.add("parentRefs", parentRefs);
        spec.add("rules", rules);

        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "HTTPRoute");
        raw.add("metadata", metadata);
        raw.add("spec", spec);
        return new DynamicKubernetesObject(raw);
    }

    private DynamicKubernetesObject buildHTTPRouteNoRules(final String routeNamespace, final String routeName) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", routeNamespace);
        metadata.addProperty("name", routeName);

        JsonObject spec = new JsonObject();

        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "HTTPRoute");
        raw.add("metadata", metadata);
        raw.add("spec", spec);
        return new DynamicKubernetesObject(raw);
    }
}
