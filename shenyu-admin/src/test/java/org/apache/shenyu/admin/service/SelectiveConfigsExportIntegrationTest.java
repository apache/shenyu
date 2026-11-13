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

package org.apache.shenyu.admin.service;

import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.dto.SelectorExportDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.admin.utils.ZipUtil;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shiro.authz.UnauthorizedException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

/**
 * Real H2 mapper/service round trips through the existing ZIP import handlers.
 */
@Transactional
public class SelectiveConfigsExportIntegrationTest extends AbstractSpringIntegrationTest {

    private static final String SOURCE = "selective-source";

    private static final String TARGET = "selective-target";

    private static final String HANDLE = "[]";

    private static final String HANDLER_PROPS = "{\"weight\":75}";

    @Resource
    private JdbcTemplate jdbc;

    @Resource
    private RuleService ruleService;

    @Resource
    private SelectiveConfigsExportService exporter;

    @Resource
    private ConfigsService configsService;

    private String pluginId;

    @BeforeEach
    public void setUp() {
        pluginId = jdbc.queryForObject("SELECT id FROM plugin WHERE name = 'divide'", String.class);
        jdbc.update("INSERT INTO namespace_plugin_rel (id, namespace_id, plugin_id, enabled, config) VALUES (?, ?, ?, true, '{}')",
                "selective-plugin", SOURCE, pluginId);
        addSelector("selected-orders", "orders", SOURCE);
        addSelector("unrelated-users", "users", SOURCE);
        addRule("orders-rule", "selected-orders", "orders", SOURCE);
        addRule("users-rule", "unrelated-users", "users", SOURCE);
        jdbc.update("INSERT INTO selector_condition (id, selector_id, param_type, operator, param_name, param_value) VALUES (?, ?, 'uri', 'match', '/', '/orders/**')",
                "orders-selector-condition", "selected-orders");
        jdbc.update("INSERT INTO rule_condition (id, rule_id, param_type, operator, param_name, param_value) VALUES (?, ?, 'uri', 'match', '/', '/orders/**')",
                "orders-rule-condition", "orders-rule");
        addDiscovery("orders", "selected-orders");
        addDiscovery("users", "unrelated-users");
    }

    @Test
    public void testLocalHttpRoundTripExcludesUnrelatedServiceAndRemapsAllDependencies() {
        try (MockedStatic<SessionUtil> session = administrator()) {
            byte[] archive = exporter.export(request());
            List<ZipUtil.ZipItem> sections = ZipUtil.unzip(archive, 10485760, 10485760, 10).getZipItemList();
            assertTrue(sections.size() >= 6 && sections.size() <= 8);
            assertFalse(sections.stream().anyMatch(item -> item.getItemData().contains("users")));
            ShenyuAdminResult result = configsService.configsImport(TARGET, archive);
            assertNotNull(result.getData());
            String selector = jdbc.queryForObject("SELECT id FROM selector WHERE namespace_id = ? AND selector_name = 'orders'", String.class, TARGET);
            assertNotEquals("selected-orders", selector);
            assertEquals(1, count("SELECT COUNT(*) FROM selector WHERE namespace_id = ?", TARGET));
            assertEquals(HANDLE, jdbc.queryForObject("SELECT handle FROM selector WHERE id = ?", String.class, selector));
            assertEquals("/orders/**", jdbc.queryForObject("SELECT param_value FROM selector_condition WHERE selector_id = ?", String.class, selector));
            String rule = jdbc.queryForObject("SELECT id FROM rule WHERE selector_id = ?", String.class, selector);
            assertNotEquals("orders-rule", rule);
            assertEquals("/orders/**", jdbc.queryForObject("SELECT param_value FROM rule_condition WHERE rule_id = ?", String.class, rule));
            assertEquals(1, count("SELECT COUNT(*) FROM rule WHERE namespace_id = ?", TARGET));
            String discovery = jdbc.queryForObject("SELECT id FROM discovery WHERE namespace_id = ? AND discovery_name = 'orders'", String.class, TARGET);
            assertNotEquals("orders-discovery", discovery);
            String handler = jdbc.queryForObject("SELECT id FROM discovery_handler WHERE discovery_id = ?", String.class, discovery);
            assertNotEquals("orders-handler", handler);
            assertEquals(HANDLER_PROPS, jdbc.queryForObject("SELECT props FROM discovery_handler WHERE id = ?", String.class, handler));
            assertEquals(selector, jdbc.queryForObject("SELECT selector_id FROM discovery_rel WHERE discovery_handler_id = ?", String.class, handler));
            assertEquals("127.0.0.1:18080", jdbc.queryForObject("SELECT upstream_url FROM discovery_upstream WHERE discovery_handler_id = ? AND namespace_id = ?",
                    String.class, handler, TARGET));
            assertEquals(1, count("SELECT COUNT(*) FROM namespace_plugin_rel WHERE namespace_id = ? AND plugin_id = ?", TARGET, pluginId));
            // The export is read-only, and does not remove either source service.
            assertEquals(2, count("SELECT COUNT(*) FROM selector WHERE namespace_id = ?", SOURCE));
            assertEquals(SOURCE, jdbc.queryForObject("SELECT namespace_id FROM selector WHERE id = 'selected-orders'", String.class));
        }
    }

    @Test
    public void testSameNameSelectorIsReusedAndExistingRuleIsPreserved() {
        addSelector("existing-orders", "orders", TARGET);
        addRule("existing-rule", "existing-orders", "orders", TARGET);
        try (MockedStatic<SessionUtil> session = administrator()) {
            final ShenyuAdminResult result = configsService.configsImport(TARGET, exporter.export(request()));
            assertEquals(1, count("SELECT COUNT(*) FROM selector WHERE namespace_id = ?", TARGET));
            assertEquals(1, count("SELECT COUNT(*) FROM rule WHERE namespace_id = ?", TARGET));
            assertEquals("existing-rule", jdbc.queryForObject("SELECT id FROM rule WHERE selector_id = 'existing-orders'", String.class));
            assertTrue(((Map<?, ?>) result.getData()).values().stream().anyMatch(value -> value.toString().contains("import fail selector: orders")));
        }
    }

    @Test
    public void testMissingPluginSchemaAndDictionaryOptionsAreImportedWithNewIds() {
        jdbc.update("INSERT INTO plugin_handle (id, plugin_id, field, label, data_type, type, sort) VALUES ('source-handle', ?, 'selective-mode', 'Mode', 3, 2, 99)", pluginId);
        jdbc.update("INSERT INTO shenyu_dict (id, type, dict_code, dict_name, dict_value, sort, enabled) "
                + "VALUES ('source-option', 'selective-mode', 'mode', 'safe', 'safe', 1, true)");
        try (MockedStatic<SessionUtil> session = administrator()) {
            byte[] archive = exporter.export(request());
            // Model a separate target Admin without these source-only global definitions.
            jdbc.update("DELETE FROM plugin_handle WHERE id = 'source-handle'");
            jdbc.update("DELETE FROM shenyu_dict WHERE id = 'source-option'");
            configsService.configsImport(TARGET, archive);
            assertNotEquals("source-handle", jdbc.queryForObject("SELECT id FROM plugin_handle WHERE plugin_id = ? AND field = 'selective-mode'", String.class, pluginId));
            assertNotEquals("source-option", jdbc.queryForObject("SELECT id FROM shenyu_dict WHERE type = 'selective-mode' AND dict_name = 'safe'", String.class));
            assertEquals("safe", jdbc.queryForObject("SELECT dict_value FROM shenyu_dict WHERE type = 'selective-mode' AND dict_name = 'safe'", String.class));
        }
    }

    @Test
    public void testAnonymousCallerCannotReadSelectedConfiguration() {
        try (MockedStatic<SessionUtil> session = mockStatic(SessionUtil.class)) {
            session.when(SessionUtil::isAdmin).thenReturn(false);
            session.when(SessionUtil::visitorId).thenReturn("-1");
            assertThrows(UnauthorizedException.class, () -> exporter.export(request()));
        }
    }

    @Test
    public void testExternalDiscoveryIsRejected() {
        jdbc.update("UPDATE discovery SET discovery_type = 'nacos' WHERE id = 'orders-discovery'");
        assertDiscoveryRejected("Only selector-local divide discovery is supported; external, shared or RPC discovery cannot be exported");
    }

    @Test
    public void testPluginLevelDiscoveryIsRejected() {
        jdbc.update("UPDATE discovery SET discovery_level = '1' WHERE id = 'orders-discovery'");
        assertDiscoveryRejected("Only selector-local divide discovery is supported; external, shared or RPC discovery cannot be exported");
    }

    @Test
    public void testSharedDiscoveryRelationIsRejected() {
        jdbc.update("INSERT INTO discovery_rel (id, plugin_name, discovery_handler_id, selector_id) VALUES ('shared-rel', 'divide', 'orders-handler', 'unrelated-users')");
        assertDiscoveryRejected("Ambiguous discovery relations are not supported");
    }

    @Test
    public void testMultipleSelectorDiscoveryBindingsAreRejected() {
        jdbc.update("UPDATE discovery_rel SET selector_id = 'selected-orders' WHERE id = 'users-rel'");
        assertDiscoveryRejected("Ambiguous selector discovery bindings are not supported");
    }

    @Test
    public void testValidCanaryWithCustomParameterDictionaryIsRejected() {
        jdbc.update("INSERT INTO shenyu_dict (id, type, dict_code, dict_name, dict_value, sort, enabled) "
                + "VALUES ('custom-param', 'paramType', 'custom-source', 'custom-source', 'custom-source', 99, true)");
        assertCanaryRejected("\"enabled\":true,\"percentage\":10,\"stickyKey\":{\"paramType\":\"custom-source\",\"paramName\":\"tenant\"}");
    }

    @Test
    public void testValidCanaryWithCustomOperatorDictionaryIsRejected() {
        jdbc.update("INSERT INTO shenyu_dict (id, type, dict_code, dict_name, dict_value, sort, enabled) "
                + "VALUES ('custom-operator', 'operator', 'custom-match', 'custom-match', 'custom-match', 99, true)");
        assertCanaryRejected("\"enabled\":true,\"percentage\":100,\"conditions\":[{\"paramType\":\"header\",\"paramName\":\"tenant\","
                + "\"operator\":\"custom-match\",\"paramValue\":\"beta\"}]");
    }

    @Test
    public void testValidDisabledCanaryIsAlsoOutsideFirstVersion() {
        assertCanaryRejected("\"enabled\":false,\"percentage\":0");
    }

    @Test
    public void testNullCanaryPreservesOrdinaryDivideRule() {
        String handle = "{\"loadBalance\":\"random\",\"canary\":null}";
        jdbc.update("UPDATE rule SET handle = ? WHERE id = 'orders-rule'", handle);
        try (MockedStatic<SessionUtil> session = administrator()) {
            configsService.configsImport(TARGET, exporter.export(request()));
            assertEquals(handle, jdbc.queryForObject("SELECT handle FROM rule WHERE namespace_id = ?", String.class, TARGET));
        }
    }

    private void assertDiscoveryRejected(final String message) {
        try (MockedStatic<SessionUtil> session = administrator()) {
            assertEquals(message, assertThrows(ShenyuException.class, () -> exporter.export(request())).getMessage());
        }
    }

    private void assertCanaryRejected(final String config) {
        String handle = "{\"loadBalance\":\"random\",\"canary\":{\"stableLabels\":{\"version\":\"stable\"},\"canaryLabels\":{\"version\":\"beta\"}," + config + "}}";
        RuleConditionDTO condition = RuleConditionDTO.builder().paramType("uri").operator("match").paramName("/").paramValue("/canary/**").build();
        RuleDTO rule = RuleDTO.builder().selectorId("selected-orders").name("canary-rule").namespaceId(SOURCE)
                .matchMode(0).sort(2).enabled(true).logged(false).matchRestful(false).handle(handle).ruleConditions(List.of(condition)).build();
        try (MockedStatic<SessionUtil> session = administrator()) {
            // Exercise real source validation, including enabled global extension dictionaries.
            assertEquals(1, ruleService.create(rule));
            assertEquals(handle, jdbc.queryForObject("SELECT handle FROM rule WHERE namespace_id = ? AND rule_name = 'canary-rule'", String.class, SOURCE));
            assertEquals("Canary rule configurations are not supported by selective export",
                    assertThrows(ShenyuException.class, () -> exporter.export(request())).getMessage());
        }
    }

    private MockedStatic<SessionUtil> administrator() {
        MockedStatic<SessionUtil> session = mockStatic(SessionUtil.class);
        session.when(SessionUtil::isAdmin).thenReturn(true);
        session.when(SessionUtil::visitorName).thenReturn("selective-export-test");
        session.when(SessionUtil::visitorId).thenReturn("1");
        return session;
    }

    private SelectorExportDTO request() {
        SelectorExportDTO request = new SelectorExportDTO();
        request.setNamespace(SOURCE);
        request.setSelectorIds(List.of("selected-orders"));
        return request;
    }

    private int count(final String sql, final Object... arguments) {
        return jdbc.queryForObject(sql, Integer.class, arguments);
    }

    private void addSelector(final String id, final String name, final String namespace) {
        jdbc.update("INSERT INTO selector (id, plugin_id, selector_name, match_mode, selector_type, sort_code, handle, enabled, logged, continued, match_restful, namespace_id) "
                        + "VALUES (?, ?, ?, 0, 1, 1, ?, true, false, false, false, ?)", id, pluginId, name, HANDLE, namespace);
    }

    private void addRule(final String id, final String selector, final String name, final String namespace) {
        jdbc.update("INSERT INTO rule (id, selector_id, rule_name, match_mode, sort_code, handle, enabled, logged, match_restful, namespace_id) "
                        + "VALUES (?, ?, ?, 0, 1, '{\"loadBalance\":\"random\"}', true, false, false, ?)", id, selector, name, namespace);
    }

    private void addDiscovery(final String name, final String selector) {
        jdbc.update("INSERT INTO discovery (id, discovery_name, discovery_level, plugin_name, namespace_id, discovery_type, props) VALUES (?, ?, '0', 'divide', ?, 'local', '{}')",
                name + "-discovery", name, SOURCE);
        jdbc.update("INSERT INTO discovery_handler (id, discovery_id, handler, props) VALUES (?, ?, '{}', ?)", name + "-handler", name + "-discovery", HANDLER_PROPS);
        jdbc.update("INSERT INTO discovery_rel (id, plugin_name, discovery_handler_id, selector_id, proxy_selector_id) VALUES (?, 'divide', ?, ?, '')",
                name + "-rel", name + "-handler", selector);
        jdbc.update("INSERT INTO discovery_upstream (id, discovery_handler_id, namespace_id, protocol, upstream_url, upstream_status, weight, props) "
                        + "VALUES (?, ?, ?, 'http', '127.0.0.1:18080', 0, 50, '{}')", name + "-upstream", name + "-handler", SOURCE);
    }
}
