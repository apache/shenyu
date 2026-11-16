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

package org.apache.shenyu.admin.service.openapi;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.admin.mapper.AgentGatewayImportMapper;
import org.apache.shenyu.admin.mapper.RuleConditionMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest.Apply;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.AgentGatewayImportDO;
import org.apache.shenyu.admin.model.entity.RuleConditionDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.result.AgentGatewayOpenApiPreview;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.openapi.impl.AgentGatewayOpenApiServiceImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** Publication selection, ownership and stale-preview guarantees. */
class AgentGatewayOpenApiServiceTest {

    private RuleMapper rules;

    private SelectorMapper selectors;

    private RuleService ruleService;

    private AgentGatewayImportMapper imports;

    private AgentGatewayOpenApiService service;

    private RuleDO target;

    private RuleDO route;

    private AtomicReference<AgentGatewayImportDO> provenance;

    @BeforeEach
    void setUp() {
        rules = mock(RuleMapper.class);
        selectors = mock(SelectorMapper.class);
        RuleConditionMapper conditions = mock(RuleConditionMapper.class);
        SelectorConditionMapper selectorConditions = mock(SelectorConditionMapper.class);
        ruleService = mock(RuleService.class);
        imports = mock(AgentGatewayImportMapper.class);
        service = new AgentGatewayOpenApiServiceImpl(rules, selectors, conditions, selectorConditions, imports, ruleService, "reserved");
        target = rule("agent", "agent-selector", "{\"trafficType\":\"mcp\",\"mcp\":{\"allowedTools\":[\"manual_provider\"]}}");
        route = rule("route", "http-selector", "{}");
        when(rules.selectById("agent")).thenReturn(target);
        when(rules.selectByIdForUpdate("agent")).thenReturn(target);
        when(rules.selectById("route")).thenReturn(route);
        when(rules.getPluginNameBySelectorId("agent-selector")).thenReturn("agentGateway");
        when(rules.getPluginNameBySelectorId("http-selector")).thenReturn("divide");
        when(selectors.selectById("agent-selector")).thenReturn(selector("agent-selector"));
        when(selectors.selectById("http-selector")).thenReturn(selector("http-selector"));
        RuleConditionDO condition = new RuleConditionDO();
        condition.setParamType("uri");
        condition.setOperator("=");
        condition.setParamValue("/orders/status");
        when(conditions.selectByQuery(any())).thenAnswer(call -> List.of(condition));
        when(selectorConditions.selectByQuery(any())).thenReturn(List.of());
        provenance = new AtomicReference<>();
        when(imports.selectByRuleId("agent", "ns")).thenAnswer(call -> provenance.get());
        when(imports.insertSelective(any())).thenAnswer(call -> {
            provenance.set(call.getArgument(0));
            return 1;
        });
        when(imports.updateManifestById(anyString(), anyString(), any())).thenAnswer(call -> {
            AgentGatewayImportDO record = provenance.get();
            record.setManifest(call.getArgument(1));
            record.setDateUpdated(call.getArgument(2));
            return 1;
        });
        when(ruleService.createOrUpdate(any())).thenAnswer(call -> {
            target.setHandle(((RuleDTO) call.getArgument(0)).getHandle());
            return 1;
        });
    }

    @Test
    void previewNeverWritesAndApplyPublishesOnlySelectedOperations() {
        AgentGatewayOpenApiRequest request = OpenApiImportFixture.request();
        AgentGatewayOpenApiPreview preview = service.preview(request);
        assertEquals(List.of("ADD", "ADD"), preview.items().stream().map(AgentGatewayOpenApiPreview.Item::action).toList());
        verify(ruleService, never()).createOrUpdate(any());
        verify(imports, never()).insertSelective(any());
        service.apply(apply(request, preview, Set.of("GET /backend/orders"), false));
        JsonObject mcp = handle().getAsJsonObject("mcp");
        assertEquals(Set.of("lookup"), mcp.getAsJsonObject("httpTools").keySet());
        assertTrue(mcp.getAsJsonArray("allowedTools").toString().contains("manual_provider"));
        assertFalse(mcp.getAsJsonArray("allowedTools").toString().contains("create"));
    }

    @Test
    void sameImportIsIdempotentAndDeselectionDoesNotRemoveOwnedTools() {
        publishGet();
        clearInvocations(ruleService);
        AgentGatewayOpenApiRequest request = OpenApiImportFixture.request();
        AgentGatewayOpenApiPreview preview = service.preview(request);
        assertEquals("UNCHANGED", preview.items().get(0).action());
        service.apply(apply(request, preview, Set.of("GET /backend/orders"), false));
        verify(ruleService, never()).createOrUpdate(any());
        AgentGatewayOpenApiRequest deselected = OpenApiImportFixture.request(OpenApiImportFixture.document(), List.of());
        assertTrue(service.preview(deselected).items().isEmpty());
        assertTrue(handle().getAsJsonObject("mcp").getAsJsonObject("httpTools").has("lookup"));
    }

    @Test
    void changedRequestMappingIsAnUpdateEvenWhenSchemaIsUnchanged() {
        publishGet();
        final AgentGatewayOpenApiRequest.Mapping mapping = new AgentGatewayOpenApiRequest.Mapping("GET", "/backend/orders", "/orders/changed", "route", null);
        // The ordinary route must change as well; this is not inferred from backend OpenAPI paths.
        final RuleConditionMapper conditions = mock(RuleConditionMapper.class);
        RuleConditionDO condition = new RuleConditionDO();
        condition.setParamType("uri");
        condition.setOperator("=");
        condition.setParamValue("/orders/changed");
        when(conditions.selectByQuery(any())).thenReturn(List.of(condition));
        service = new AgentGatewayOpenApiServiceImpl(rules, selectors, conditions, mock(SelectorConditionMapper.class), imports, ruleService, "");
        AgentGatewayOpenApiPreview preview = service.preview(OpenApiImportFixture.request(OpenApiImportFixture.document(), List.of(mapping)));
        assertEquals("UPDATE", preview.items().get(0).action());
        assertTrue(preview.items().get(0).before().contains("/orders/status"));
        assertTrue(preview.items().get(0).after().contains("/orders/changed"));
    }

    @Test
    void manualEditsAndOtherSourcesBecomeConflicts() {
        publishGet();
        target.setHandle(target.getHandle().replace("Look up an order", "Manually changed"));
        AgentGatewayOpenApiRequest request = OpenApiImportFixture.request();
        AgentGatewayOpenApiPreview preview = service.preview(request);
        assertEquals("CONFLICT", preview.items().get(0).action());
        assertThrows(IllegalArgumentException.class, () -> service.apply(apply(request, preview, Set.of("GET /backend/orders"), false)));
        AgentGatewayOpenApiRequest otherSource = new AgentGatewayOpenApiRequest("ns", "agent", "another-source", request.document(), request.mappings());
        assertEquals("CONFLICT", service.preview(otherSource).items().get(0).action());
    }

    @Test
    void actualSourceRemovalRequiresSeparateConfirmationAndPreservesOtherGrants() {
        publishGet();
        JsonObject source = OpenApiImportFixture.document();
        source.getAsJsonObject("paths").getAsJsonObject("/backend/orders").remove("get");
        AgentGatewayOpenApiRequest request = OpenApiImportFixture.request(source, List.of());
        AgentGatewayOpenApiPreview preview = service.preview(request);
        assertEquals("REMOVE", preview.items().get(0).action());
        assertThrows(IllegalArgumentException.class, () -> service.apply(apply(request, preview, Set.of("GET /backend/orders"), false)));
        service.apply(apply(request, preview, Set.of("GET /backend/orders"), true));
        assertTrue(handle().getAsJsonObject("mcp").getAsJsonObject("httpTools").isEmpty());
        assertEquals("[\"manual_provider\"]", handle().getAsJsonObject("mcp").get("allowedTools").toString());
    }

    @Test
    void staleRuleOrRoutePreviewsCannotPublish() {
        AgentGatewayOpenApiRequest request = OpenApiImportFixture.request();
        AgentGatewayOpenApiPreview preview = service.preview(request);
        route.setHandle("{\"upstream\":\"changed\"}");
        Apply staleRoute = apply(request, preview, Set.of("GET /backend/orders"), false);
        assertThrows(IllegalArgumentException.class, () -> service.apply(staleRoute));
        preview = service.preview(request);
        target.setHandle(target.getHandle().replace("manual_provider", "different_provider"));
        Apply stale = apply(request, preview, Set.of("GET /backend/orders"), false);
        assertThrows(IllegalArgumentException.class, () -> service.apply(stale));
        verify(ruleService, never()).createOrUpdate(any());
    }

    @Test
    void namespaceAndRouteOwnershipAreChecked() {
        route.setNamespaceId("other");
        assertEquals("CONFLICT", service.preview(OpenApiImportFixture.request()).items().get(0).action());
        target.setNamespaceId("other");
        assertThrows(IllegalArgumentException.class, () -> service.preview(OpenApiImportFixture.request()));
    }

    @Test
    void rejectsSelectionThatExceedsTheMergedToolBudget() {
        target.setHandle(handleWithTools(15));
        AgentGatewayOpenApiRequest request = OpenApiImportFixture.request();
        AgentGatewayOpenApiPreview preview = service.preview(request);
        IllegalArgumentException error = assertThrows(IllegalArgumentException.class,
                () -> service.apply(apply(request, preview, Set.of("GET /backend/orders", "POST /backend/orders"), false)));
        assertTrue(error.getMessage().contains("Tool budget exceeded"));
        verify(ruleService, never()).createOrUpdate(any());
        verify(imports, never()).insertSelective(any());
    }

    @Test
    void allowsSelectionAtTheExactToolBudget() {
        target.setHandle(handleWithTools(15));
        AgentGatewayOpenApiRequest request = OpenApiImportFixture.request();
        AgentGatewayOpenApiPreview preview = service.preview(request);
        service.apply(apply(request, preview, Set.of("GET /backend/orders"), false));
        assertEquals(16, handle().getAsJsonObject("mcp").getAsJsonObject("httpTools").size());
    }

    @Test
    void unknownSelectionsAndUnacknowledgedCatalogCannotPublish() {
        AgentGatewayOpenApiRequest request = OpenApiImportFixture.request();
        AgentGatewayOpenApiPreview preview = service.preview(request);
        assertTrue(preview.warnings().get(0).contains("EXTERNAL_CATALOG_UNVERIFIED"));
        assertThrows(IllegalArgumentException.class, () -> service.apply(apply(request, preview, Set.of("unknown"), false)));
        assertThrows(IllegalArgumentException.class, () -> service.apply(new Apply(request, preview.revision(), Set.of("GET /backend/orders"), false, false)));
    }

    private void publishGet() {
        AgentGatewayOpenApiRequest request = OpenApiImportFixture.request();
        service.apply(apply(request, service.preview(request), Set.of("GET /backend/orders"), false));
    }

    private Apply apply(final AgentGatewayOpenApiRequest request, final AgentGatewayOpenApiPreview preview, final Set<String> selected, final boolean removals) {
        return new Apply(request, preview.revision(), selected, removals, true);
    }

    private JsonObject handle() {
        return JsonParser.parseString(target.getHandle()).getAsJsonObject();
    }

    private String handleWithTools(final int count) {
        JsonObject mcp = new JsonObject();
        JsonArray allowed = new JsonArray();
        JsonObject tools = new JsonObject();
        for (int index = 0; index < count; index++) {
            String name = "existing" + index;
            allowed.add(name);
            tools.add(name, JsonParser.parseString("{\"description\":\"existing\","
                    + "\"inputSchema\":{\"type\":\"object\",\"properties\":{}},"
                    + "\"requestConfig\":{\"requestTemplate\":{\"method\":\"GET\",\"url\":\"/existing\"},"
                    + "\"argsPosition\":{}}}"));
        }
        mcp.add("allowedTools", allowed);
        mcp.add("httpTools", tools);
        JsonObject handle = new JsonObject();
        handle.addProperty("trafficType", "mcp");
        handle.add("mcp", mcp);
        return handle.toString();
    }

    private RuleDO rule(final String id, final String selector, final String handle) {
        RuleDO rule = new RuleDO(selector, 0, id, true, false, 0, handle, false, "ns");
        rule.setId(id);
        return rule;
    }

    private SelectorDO selector(final String id) {
        SelectorDO selector = new SelectorDO();
        selector.setId(id);
        selector.setNamespaceId("ns");
        selector.setEnabled(true);
        selector.setSelectorType(0);
        return selector;
    }
}
