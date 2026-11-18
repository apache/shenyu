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

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import io.kubernetes.client.common.KubernetesObject;
import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.workqueue.RateLimitingQueue;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Indexer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubset;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1Namespace;
import io.kubernetes.client.openapi.models.V1ObjectMeta;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.apache.shenyu.k8s.reconciler.HTTPRouteReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** Namespace authorization regressions through the real HTTPRoute parser. */
public final class HTTPRouteNamespaceRevocationTest {

    @BeforeEach
    public void setUp() {
        GatewayRouteCache.getInstance().clear();
    }

    @AfterEach
    public void tearDown() {
        GatewayRouteCache.getInstance().clear();
    }

    /** Revoking one parent removes its host's selector and rules while the other parent remains usable. */
    @Test
    public void testRevocationDeletesOnlyTheRejectedParentsHostConfiguration() throws Exception {
        Fixture fixture = new Fixture("gateways");
        fixture.addAllParent();
        fixture.namespaceLabels(Map.of("team", "blue"));
        fixture.reconcile();

        ArgumentCaptor<SelectorData> selectors = ArgumentCaptor.forClass(SelectorData.class);
        ArgumentCaptor<RuleData> rules = ArgumentCaptor.forClass(RuleData.class);
        verify(fixture.repository, times(2)).saveOrUpdateSelectorData(selectors.capture());
        verify(fixture.repository, times(2)).saveOrUpdateRuleData(rules.capture());
        SelectorData selectedHost = selectorForHost(selectors.getAllValues(), "selected.example.com");
        SelectorData allHost = selectorForHost(selectors.getAllValues(), "all.example.com");
        Assertions.assertNotEquals(selectedHost.getId(), allHost.getId());
        Assertions.assertEquals(Set.of(selectedHost.getId(), allHost.getId()),
                Set.copyOf(fixture.cache.getRouteSelectors("routes", "test-route", "divide")));
        Assertions.assertEquals(Set.of(selectedHost.getId(), allHost.getId()),
                Set.of(rules.getAllValues().get(0).getSelectorId(), rules.getAllValues().get(1).getSelectorId()));
        fixture.assertAccepted(0, "True", "Accepted");
        fixture.assertAccepted(1, "True", "Accepted");
        Assertions.assertEquals(Set.of("gateways/selected", "gateways/all"), fixture.cache.getGatewaysForRoute("routes", "test-route"));

        clearInvocations(fixture.repository, fixture.gatewayQueue);
        fixture.namespaceLabels(Map.of("team", "red"));
        fixture.reconcile();

        verify(fixture.repository).deleteSelectorWithRules("divide", selectedHost.getId());
        verify(fixture.repository, never()).deleteSelectorWithRules("divide", allHost.getId());
        ArgumentCaptor<SelectorData> survivingSelector = ArgumentCaptor.forClass(SelectorData.class);
        ArgumentCaptor<RuleData> survivingRule = ArgumentCaptor.forClass(RuleData.class);
        verify(fixture.repository).saveOrUpdateSelectorData(survivingSelector.capture());
        verify(fixture.repository).saveOrUpdateRuleData(survivingRule.capture());
        Assertions.assertEquals(allHost.getId(), survivingSelector.getValue().getId());
        Assertions.assertEquals(allHost.getId(), survivingRule.getValue().getSelectorId());
        RuleData originalRule = rules.getAllValues().stream()
                .filter(rule -> allHost.getId().equals(rule.getSelectorId())).findFirst().orElseThrow();
        Assertions.assertEquals(originalRule.getId(), survivingRule.getValue().getId());
        Assertions.assertEquals(List.of(allHost.getId()), fixture.cache.getRouteSelectors("routes", "test-route", "divide"));
        Assertions.assertEquals(Set.of("gateways/all"), fixture.cache.getGatewaysForRoute("routes", "test-route"));
        Assertions.assertTrue(fixture.cache.getRoutesByListener("gateways", "selected", "http").isEmpty());
        Assertions.assertEquals(Set.of("routes/test-route"), fixture.cache.getRoutesByListener("gateways", "all", "http"));
        fixture.assertAccepted(0, "False", "NotAllowedByListeners");
        fixture.assertAccepted(1, "True", "Accepted");
        verify(fixture.gatewayQueue).add(new Request("gateways", "selected"));
        verify(fixture.gatewayQueue).add(new Request("gateways", "all"));
    }

    /** All expression operators control runtime attachment using Namespace labels, including label absence. */
    @Test
    public void testExpressionOperatorsAuthorizeAndRevokeAtReconcile() throws Exception {
        List<String> operators = List.of("In", "NotIn", "Exists", "DoesNotExist");
        for (String operator : operators) {
            GatewayRouteCache.getInstance().clear();
            final Fixture fixture = new Fixture("gateways");
            JsonObject expression = new JsonObject();
            expression.addProperty("key", "team");
            expression.addProperty("operator", operator);
            if ("In".equals(operator) || "NotIn".equals(operator)) {
                expression.add("values", JsonParser.parseString("[\"blue\"]"));
            }
            JsonObject selector = new JsonObject();
            selector.add("matchExpressions", JsonParser.parseString("[" + expression + "]"));
            fixture.namespaces().add("selector", selector);
            Map<String, String> permitted = "NotIn".equals(operator) ? Map.of("team", "red")
                    : "DoesNotExist".equals(operator) ? Map.of() : Map.of("team", "blue");
            final Map<String, String> denied = "In".equals(operator) ? Map.of("team", "red")
                    : "Exists".equals(operator) ? Map.of() : Map.of("team", "blue");
            fixture.namespaceLabels(permitted);
            fixture.reconcile();
            verify(fixture.repository).saveOrUpdateSelectorData(any());
            verify(fixture.repository).saveOrUpdateRuleData(any());
            fixture.assertAccepted(0, "True", "Accepted");
            final String selectorId = fixture.cache.getRouteSelectors("routes", "test-route", "divide").get(0);

            clearInvocations(fixture.repository);
            fixture.namespaceLabels(denied);
            fixture.reconcile();
            verify(fixture.repository).deleteSelectorWithRules("divide", selectorId);
            verify(fixture.repository, never()).saveOrUpdateSelectorData(any());
            verify(fixture.repository, never()).saveOrUpdateRuleData(any());
            Assertions.assertNull(fixture.cache.getGatewaysForRoute("routes", "test-route"), operator);
            fixture.assertAccepted(0, "False", "NotAllowedByListeners");
        }
    }

    /** Selector authorization applies even when the route and Gateway share a Namespace. */
    @Test
    public void testSameNamespaceCannotBypassSelector() throws Exception {
        Fixture fixture = new Fixture("routes");
        fixture.namespaceLabels(Map.of("team", "red"));
        fixture.reconcile();
        verify(fixture.repository, never()).saveOrUpdateSelectorData(any());
        verify(fixture.repository, never()).saveOrUpdateRuleData(any());
        Assertions.assertNull(fixture.cache.getGatewaysForRoute("routes", "test-route"));
        fixture.assertAccepted(0, "False", "NotAllowedByListeners");
    }

    private SelectorData selectorForHost(final List<SelectorData> selectors, final String hostname) {
        return selectors.stream().filter(selector -> selector.getConditionList().stream()
                .anyMatch(condition -> hostname.equals(condition.getParamValue()))).findFirst().orElseThrow();
    }

    private DynamicKubernetesObject gateway(final String namespace, final String name, final String hostname, final String from) {
        JsonObject raw = JsonParser.parseString("{\"apiVersion\":\"gateway.networking.k8s.io/v1\",\"kind\":\"Gateway\","
                + "\"metadata\":{},\"spec\":{\"gatewayClassName\":\"shenyu\",\"listeners\":[{\"name\":\"http\","
                + "\"protocol\":\"HTTP\",\"port\":9195,\"allowedRoutes\":{\"namespaces\":{}}}]}}").getAsJsonObject();
        raw.getAsJsonObject("metadata").addProperty("namespace", namespace);
        raw.getAsJsonObject("metadata").addProperty("name", name);
        JsonObject listener = raw.getAsJsonObject("spec").getAsJsonArray("listeners").get(0).getAsJsonObject();
        listener.addProperty("hostname", hostname);
        listener.getAsJsonObject("allowedRoutes").getAsJsonObject("namespaces").addProperty("from", from);
        return new DynamicKubernetesObject(raw);
    }

    private final class Fixture {

        private final GatewayRouteCache cache = GatewayRouteCache.getInstance();

        private final Indexer<V1Namespace> namespaceIndexer = mock(Indexer.class);

        private final Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);

        private final DynamicKubernetesObject selected;

        private final DynamicKubernetesObject route;

        private final ShenyuCacheRepository repository = mock(ShenyuCacheRepository.class);

        private final RateLimitingQueue<Request> gatewayQueue = mock(RateLimitingQueue.class);

        private final ArgumentCaptor<Object> patches = ArgumentCaptor.forClass(Object.class);

        private final HTTPRouteReconciler reconciler;

        private Fixture(final String gatewayNamespace) throws Exception {
            route = new DynamicKubernetesObject(JsonParser.parseString("{\"apiVersion\":\"gateway.networking.k8s.io/v1\",\"kind\":\"HTTPRoute\","
                    + "\"metadata\":{\"namespace\":\"routes\",\"name\":\"test-route\"},\"spec\":{\"parentRefs\":[],"
                    + "\"hostnames\":[\"selected.example.com\",\"all.example.com\"],\"rules\":[{\"backendRefs\":[{\"name\":\"service\",\"port\":80}]}]}}")
                    .getAsJsonObject());
            JsonObject parent = new JsonObject();
            parent.addProperty("namespace", gatewayNamespace);
            parent.addProperty("name", "selected");
            route.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs").add(parent);
            selected = gateway(gatewayNamespace, "selected", "selected.example.com", "Selector");
            namespaces().add("selector", JsonParser.parseString("{\"matchLabels\":{\"team\":\"blue\"}}"));
            when(gatewayIndexer.getByKey(gatewayNamespace + "/selected")).thenReturn(selected);

            Indexer<DynamicKubernetesObject> routeIndexer = mock(Indexer.class);
            when(routeIndexer.getByKey("routes/test-route")).thenReturn(route);
            Indexer<DynamicKubernetesObject> classIndexer = mock(Indexer.class);
            when(classIndexer.getByKey("shenyu")).thenReturn(new DynamicKubernetesObject(JsonParser.parseString(
                    "{\"metadata\":{\"name\":\"shenyu\"},\"spec\":{\"controllerName\":\"gateway.shenyu.apache.org/shenyu-controller\"}}").getAsJsonObject()));
            Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
            V1Endpoints endpoints = new V1Endpoints().metadata(new V1ObjectMeta().namespace("routes").name("service"))
                    .subsets(List.of(new V1EndpointSubset().addresses(List.of(new V1EndpointAddress().ip("127.0.0.1")))));
            when(endpointsIndexer.getByKey("routes/service")).thenReturn(endpoints);
            HttpRouteParser parser = new HttpRouteParser(new Lister<>(endpointsIndexer),
                    new Lister<>(mock(Indexer.class)), new Lister<>(mock(Indexer.class)));
            ApiClient client = mock(ApiClient.class);
            when(client.getAuthentications()).thenReturn(Map.of());
            when(client.buildCall(any(), any(), any(), any(), patches.capture(), any(), any(), any(), any(), any()))
                    .thenReturn(mock(okhttp3.Call.class));
            reconciler = new HTTPRouteReconciler(informer(routeIndexer), informer(gatewayIndexer), informer(classIndexer),
                    informer(namespaceIndexer), parser, repository, client, 9195, gatewayQueue);
        }

        private <T extends KubernetesObject> SharedIndexInformer<T> informer(final Indexer<T> indexer) {
            SharedIndexInformer<T> informer = mock(SharedIndexInformer.class);
            when(informer.getIndexer()).thenReturn(indexer);
            return informer;
        }

        private JsonObject namespaces() {
            return selected.getRaw().getAsJsonObject("spec").getAsJsonArray("listeners").get(0).getAsJsonObject()
                    .getAsJsonObject("allowedRoutes").getAsJsonObject("namespaces");
        }

        private void addAllParent() {
            when(gatewayIndexer.getByKey("gateways/all")).thenReturn(gateway("gateways", "all", "all.example.com", "All"));
            route.getRaw().getAsJsonObject("spec").getAsJsonArray("parentRefs")
                    .add(JsonParser.parseString("{\"namespace\":\"gateways\",\"name\":\"all\"}"));
        }

        private void namespaceLabels(final Map<String, String> labels) {
            when(namespaceIndexer.getByKey("routes")).thenReturn(new V1Namespace().metadata(new V1ObjectMeta().name("routes").labels(labels)));
        }

        private void reconcile() {
            reconciler.reconcile(new Request("routes", "test-route"));
        }

        private void assertAccepted(final int parentIndex, final String status, final String reason) {
            JsonObject parent = ((JsonObject) patches.getValue()).getAsJsonObject("status").getAsJsonArray("parents").get(parentIndex).getAsJsonObject();
            JsonObject condition = parent.getAsJsonArray("conditions").get(0).getAsJsonObject();
            Assertions.assertEquals(status, condition.get("status").getAsString());
            Assertions.assertEquals(reason, condition.get("reason").getAsString());
        }
    }
}
