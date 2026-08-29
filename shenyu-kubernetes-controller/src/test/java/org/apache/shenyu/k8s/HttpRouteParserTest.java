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
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
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
     * Test parse with path prefix match: the spec requires element-boundary matching
     * ({@code /api} matches {@code /api} and {@code /api/x} but not {@code /apix}), so the
     * prefix maps to an anchored regex, not a raw startsWith. Longer prefixes must sort
     * lower (higher precedence) than shorter ones and any exact path must outrank a prefix.
     */
    @Test
    public void testParseWithPathPrefix() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister, mockReferenceGrantLister());

        DynamicKubernetesObject httpRoute = buildHTTPRoute(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/api", "PathPrefix", null, null);
        ShenyuMemoryConfig config = parser.parse(httpRoute, List.of());

        List<SelectorData> selectors = extractSelectors(config);
        List<RuleData> rules = extractRules(config);
        Assertions.assertEquals(1, selectors.size());
        Assertions.assertEquals(1, rules.size());

        SelectorData selector = config.getRouteConfigList().get(0).getSelectorData();
        ConditionData pathCondition = selector.getConditionList().get(0);
        Assertions.assertEquals(ParamTypeEnum.URI.getName(), pathCondition.getParamType());
        Assertions.assertEquals(OperatorEnum.REGEX.getAlias(), pathCondition.getOperator());
        Assertions.assertEquals("^\\Q/api\\E(/.*)?$", pathCondition.getParamValue());
        // prefix sort = 1000 - min(len, 800): below exact (100), above regex (2000)
        Assertions.assertEquals(1000 - "/api".length(), selector.getSort());
    }

    /**
     * Test parse with exact path match: EQ operator and the highest-precedence sort,
     * outranking any prefix/regex/no-path match per the spec precedence rules.
     */
    @Test
    public void testParseWithExactPath() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister, mockReferenceGrantLister());

        DynamicKubernetesObject httpRoute = buildHTTPRoute(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/api/v1/test", "Exact", null, null);
        ShenyuMemoryConfig config = parser.parse(httpRoute, List.of());

        SelectorData selector = config.getRouteConfigList().get(0).getSelectorData();
        ConditionData pathCondition = selector.getConditionList().get(0);
        Assertions.assertEquals(OperatorEnum.EQ.getAlias(), pathCondition.getOperator());
        Assertions.assertEquals(100, selector.getSort());
    }

    /**
     * A rule without matches is the catch-all of the spec (equivalent to PathPrefix /). A
     * CUSTOM_FLOW selector with no conditions never matches in ShenYu, so the parser must
     * emit an explicit match-all condition instead of a dead selector.
     */
    @Test
    public void testParseWithRuleWithoutMatchesGetsMatchAllCondition() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister, mockReferenceGrantLister());

        DynamicKubernetesObject httpRoute = buildHTTPRoute(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/api", "PathPrefix", null, null);
        httpRoute.getRaw().getAsJsonObject("spec")
                .getAsJsonArray("rules").get(0).getAsJsonObject()
                .remove("matches");
        ShenyuMemoryConfig config = parser.parse(httpRoute, List.of());

        Assertions.assertEquals(1, config.getRouteConfigList().size());
        SelectorData selector = config.getRouteConfigList().get(0).getSelectorData();
        Assertions.assertEquals(1, selector.getConditionList().size());
        ConditionData condition = selector.getConditionList().get(0);
        Assertions.assertEquals(ParamTypeEnum.URI.getName(), condition.getParamType());
        Assertions.assertEquals(OperatorEnum.STARTS_WITH.getAlias(), condition.getOperator());
        Assertions.assertEquals("/", condition.getParamValue());
    }

    /**
     * Test parse with hostname conditions.
     * Each hostname should generate a separate selector+rule (one hostname per selector).
     */
    @Test
    public void testParseWithHostnames() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister, mockReferenceGrantLister());

        DynamicKubernetesObject httpRoute = buildHTTPRouteWithHostnames(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/**", "PathPrefix", new String[]{"example.com", "api.example.com"});
        ShenyuMemoryConfig config = parser.parse(httpRoute, List.of("example.com", "api.example.com"));

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
     * Test that exact hostnames use EQ and wildcard hostnames use REGEX with an anchored
     * suffix-match regex: per the Gateway API spec {@code *.example.com} matches
     * {@code test.example.com} and {@code foo.test.example.com} but not {@code example.com}.
     */
    @Test
    public void testParseWithWildcardHostname() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister, mockReferenceGrantLister());

        DynamicKubernetesObject httpRoute = buildHTTPRouteWithHostnames(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/**", "PathPrefix", new String[]{"*.example.com"});
        ShenyuMemoryConfig config = parser.parse(httpRoute, List.of("*.example.com"));

        List<ConditionData> domainConditions = config.getRouteConfigList().stream()
                .flatMap(rc -> rc.getSelectorData().getConditionList().stream())
                .filter(c -> ParamTypeEnum.DOMAIN.getName().equals(c.getParamType()))
                .toList();
        Assertions.assertEquals(1, domainConditions.size());

        ConditionData wildcardCondition = domainConditions.get(0);
        Assertions.assertEquals(OperatorEnum.REGEX.getAlias(), wildcardCondition.getOperator());
        Assertions.assertEquals("^([^.]+\\.)+example\\.com$", wildcardCondition.getParamValue());
    }

    /**
     * Test parse with header match.
     */
    @Test
    public void testParseWithHeaderMatch() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister, mockReferenceGrantLister());

        DynamicKubernetesObject httpRoute = buildHTTPRoute(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/**", "PathPrefix", "X-Custom-Header", "test-value");
        ShenyuMemoryConfig config = parser.parse(httpRoute, List.of());

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
     * A method match must produce a req_method/EQ condition instead of being silently
     * dropped: dropping it would widen the selector to match requests of any method.
     */
    @Test
    public void testParseWithMethodMatch() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister, mockReferenceGrantLister());

        DynamicKubernetesObject httpRoute = buildHTTPRoute(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/**", "PathPrefix", null, null);
        httpRoute.getRaw().getAsJsonObject("spec")
                .getAsJsonArray("rules").get(0).getAsJsonObject()
                .getAsJsonArray("matches").get(0).getAsJsonObject()
                .addProperty("method", "GET");
        ShenyuMemoryConfig config = parser.parse(httpRoute, List.of());

        ConditionData methodCondition = config.getRouteConfigList().get(0).getSelectorData().getConditionList().stream()
                .filter(c -> ParamTypeEnum.REQUEST_METHOD.getName().equals(c.getParamType()))
                .findFirst().orElse(null);
        Assertions.assertNotNull(methodCondition);
        Assertions.assertEquals(OperatorEnum.EQ.getAlias(), methodCondition.getOperator());
        Assertions.assertEquals("GET", methodCondition.getParamValue());
    }

    /**
     * Test parse with query param match.
     */
    @Test
    public void testParseWithQueryParam() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister, mockReferenceGrantLister());

        DynamicKubernetesObject httpRoute = buildHTTPRouteWithQueryParams(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/**", "PathPrefix", "debug", "true", "Exact");
        ShenyuMemoryConfig config = parser.parse(httpRoute, List.of());

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
     * A backendRef with an explicit weight mixed with one that omits it must resolve to the
     * spec default 1 (9:1), not to an arbitrary parser default like 100 (9:100). The weight
     * is per-backend per the spec, so it is spread across the backend's endpoints — copying
     * it to every endpoint would multiply the backend's share by its replica count. Multiple
     * backendRefs of one rule fan out into a single selector's upstream list.
     */
    @Test
    public void testMixedExplicitAndDefaultBackendRefWeights() {
        DynamicKubernetesObject httpRoute = buildHTTPRoute(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/**", "PathPrefix", null, null);
        JsonArray backendRefs = httpRoute.getRaw().getAsJsonObject("spec")
                .getAsJsonArray("rules").get(0).getAsJsonObject().getAsJsonArray("backendRefs");
        backendRefs.get(0).getAsJsonObject().addProperty("weight", 9);
        JsonObject unweighted = new JsonObject();
        unweighted.addProperty("name", SERVICE_NAME);
        unweighted.addProperty("port", 8080);
        backendRefs.add(unweighted);

        HttpRouteParser parser = new HttpRouteParser(mockEndpointsLister(), mockReferenceGrantLister());
        ShenyuMemoryConfig config = parser.parse(httpRoute, List.of());

        List<SelectorData> selectors = extractSelectors(config);
        Assertions.assertEquals(1, selectors.size());

        List<DivideUpstream> upstreams = GsonUtils.getInstance()
                .fromList(selectors.get(0).getHandle(), DivideUpstream.class);
        // Each backendRef fans out to the 2 addresses of the mocked Endpoints; the
        // per-backend weights 9 and 1 are halved across their 2 endpoints (floored at 1)
        Assertions.assertEquals(4, upstreams.size());
        Assertions.assertEquals(4, upstreams.get(0).getWeight());
        Assertions.assertEquals(4, upstreams.get(1).getWeight());
        Assertions.assertEquals(1, upstreams.get(2).getWeight());
        Assertions.assertEquals(1, upstreams.get(3).getWeight());
    }

    /**
     * Deterministic ID: parsing the same HTTPRoute twice must yield identical selector/rule IDs.
     * This is the core guarantee that makes reconcile idempotent under informer resync —
     * without it every resync would delete and recreate selectors, briefly leaving routes
     * unmatched on the data plane.
     */
    @Test
    public void testSelectorAndRuleIdsAreStableAcrossParses() {
        Lister<V1Endpoints> endpointsLister = mockEndpointsLister();
        HttpRouteParser parser = new HttpRouteParser(endpointsLister, mockReferenceGrantLister());

        DynamicKubernetesObject httpRoute = buildHTTPRouteWithHostnames(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/**", "PathPrefix", new String[]{"example.com", "api.example.com"});

        ShenyuMemoryConfig first = parser.parse(httpRoute, List.of("example.com", "api.example.com"));
        List<String> firstSelectorIds = first.getRouteConfigList().stream()
                .map(rc -> rc.getSelectorData().getId()).toList();
        List<String> firstRuleIds = first.getRouteConfigList().stream()
                .flatMap(rc -> rc.getRuleDataList().stream()).map(r -> r.getId()).toList();

        ShenyuMemoryConfig second = parser.parse(httpRoute, List.of("example.com", "api.example.com"));
        List<String> secondSelectorIds = second.getRouteConfigList().stream()
                .map(rc -> rc.getSelectorData().getId()).toList();
        List<String> secondRuleIds = second.getRouteConfigList().stream()
                .flatMap(rc -> rc.getRuleDataList().stream()).map(r -> r.getId()).toList();

        Assertions.assertEquals(firstSelectorIds, secondSelectorIds, "selector IDs must be deterministic");
        Assertions.assertEquals(firstRuleIds, secondRuleIds, "rule IDs must be deterministic");
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

    /**
     * Empty ReferenceGrant lister: same-namespace backendRefs never consult grants,
     * so all existing parser tests are unaffected by grant matching.
     */
    private Lister<DynamicKubernetesObject> mockReferenceGrantLister() {
        return new Lister<>(mock(Indexer.class));
    }

    /**
     * Cross-namespace backendRef WITH a matching ReferenceGrant (to Service in the core
     * group) resolves normally.
     */
    @Test
    public void testCrossNamespaceBackendRefWithGrantResolves() {
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints endpoints = new V1EndpointsBuilder()
                .withNewMetadata().withNamespace("other-ns").withName(SERVICE_NAME).endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("10.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("other-ns/" + SERVICE_NAME)).thenReturn(endpoints);
        Lister<V1Endpoints> endpointsLister = new Lister<>(endpointsIndexer);

        Indexer<DynamicKubernetesObject> grantIndexer = mock(Indexer.class);
        when(grantIndexer.byIndex("namespace", "other-ns")).thenReturn(List.of(buildServiceGrant("other-ns", NAMESPACE, null)));
        HttpRouteParser parser = new HttpRouteParser(endpointsLister, new Lister<>(grantIndexer));

        DynamicKubernetesObject httpRoute = buildHTTPRoute(NAMESPACE, "test-route",
                NAMESPACE, "shenyu-gateway", SERVICE_NAME, SERVICE_PORT,
                "/api/**", "PathPrefix", null, null);
        JsonObject backendRef = httpRoute.getRaw().getAsJsonObject("spec")
                .getAsJsonArray("rules").get(0).getAsJsonObject()
                .getAsJsonArray("backendRefs").get(0).getAsJsonObject();
        backendRef.addProperty("namespace", "other-ns");

        ShenyuMemoryConfig config = parser.parse(httpRoute, List.of());

        Assertions.assertTrue(config.isAllBackendsResolved());
        Assertions.assertEquals(1, extractSelectors(config).size());
    }

    /**
     * Build a ReferenceGrant allowing an HTTPRoute from {@code fromNamespace} to
     * reference a Service (core group, group="") in {@code namespace}, optionally
     * restricted to {@code toName}.
     */
    private DynamicKubernetesObject buildServiceGrant(final String namespace, final String fromNamespace,
                                                      final String toName) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", namespace);
        metadata.addProperty("name", "allow-services");

        JsonObject from = new JsonObject();
        from.addProperty("group", "gateway.networking.k8s.io");
        from.addProperty("kind", "HTTPRoute");
        from.addProperty("namespace", fromNamespace);
        JsonArray fromArray = new JsonArray();
        fromArray.add(from);

        JsonObject to = new JsonObject();
        to.addProperty("group", "");
        to.addProperty("kind", "Service");
        if (Objects.nonNull(toName)) {
            to.addProperty("name", toName);
        }
        JsonArray toArray = new JsonArray();
        toArray.add(to);

        JsonObject spec = new JsonObject();
        spec.add("from", fromArray);
        spec.add("to", toArray);

        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1beta1");
        raw.addProperty("kind", "ReferenceGrant");
        raw.add("metadata", metadata);
        raw.add("spec", spec);
        return new DynamicKubernetesObject(raw);
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

}
