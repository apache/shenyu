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

package org.apache.shenyu.admin.controller;

import com.jayway.jsonpath.JsonPath;
import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shiro.web.servlet.AbstractShiroFilter;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Selector and rule writes of non-admin users, checked with the Shiro filter, a real login and the
 * grants stored in the database.
 */
public final class PluginDataPermissionIntegrationTest extends AbstractSpringIntegrationTest {

    private static final String PREFIX = "perm-it-";

    private static final String NAMESPACE = Constants.SYS_DEFAULT_NAMESPACE_ID;

    private static final String PASSWORD = "123456";

    private static final String CONDITION = "{\"paramType\":\"uri\",\"operator\":\"match\",\"paramName\":\"/\",\"paramValue\":\"/perm-it/**\"}";

    private static final String[] WRITE_ACTIONS = {"Selector:add", "Selector:edit", "Selector:delete", "Rule:add", "Rule:edit", "Rule:delete"};

    @Resource
    private WebApplicationContext context;

    @Resource
    private JdbcTemplate jdbcTemplate;

    private MockMvc mockMvc;

    private String dividePluginId;

    private String grpcPluginId;

    private int sequence;

    @BeforeEach
    public void setUp() {
        // unit tests replace this shared holder with a mock context, and a cached application context does not set it again
        SpringBeanUtils.getInstance().setApplicationContext(context);
        mockMvc = MockMvcBuilders.webAppContextSetup(context)
                .addFilters(context.getBean(AbstractShiroFilter.class))
                .build();
        dividePluginId = jdbcTemplate.queryForObject("SELECT id FROM plugin WHERE name = 'divide'", String.class);
        grpcPluginId = jdbcTemplate.queryForObject("SELECT id FROM plugin WHERE name = 'grpc'", String.class);
    }

    @AfterEach
    public void cleanUp() {
        final String like = PREFIX + "%";
        jdbcTemplate.update("DELETE FROM rule_condition WHERE rule_id IN (SELECT id FROM rule WHERE rule_name LIKE ?)", like);
        jdbcTemplate.update("DELETE FROM selector_condition WHERE selector_id IN (SELECT id FROM selector WHERE selector_name LIKE ?)", like);
        jdbcTemplate.update("DELETE FROM rule WHERE rule_name LIKE ?", like);
        jdbcTemplate.update("DELETE FROM selector WHERE selector_name LIKE ?", like);
        jdbcTemplate.update("DELETE FROM data_permission WHERE user_id LIKE ?", like);
        jdbcTemplate.update("DELETE FROM permission WHERE object_id LIKE ?", like);
        jdbcTemplate.update("DELETE FROM user_role WHERE user_id LIKE ?", like);
        jdbcTemplate.update("DELETE FROM role WHERE id LIKE ?", like);
        jdbcTemplate.update("DELETE FROM dashboard_user WHERE id LIKE ?", like);
    }

    @Test
    public void testPluginGrantsAllowWritesOnThatPluginOnly() throws Exception {
        final String token = loginWithGrants("divide", pluginGrants("divide"));
        final String adminToken = login("admin");

        // every write of the granted plugin is accepted
        assertEquals(CommonErrorCode.SUCCESSFUL, send(token, HttpMethod.POST, "/selector", selector(dividePluginId, "divide-s")));
        final String selectorId = selectorId("divide-s");
        assertEquals(CommonErrorCode.SUCCESSFUL, send(token, HttpMethod.PUT, "/selector/" + selectorId, selector(dividePluginId, "divide-s")));
        assertEquals(CommonErrorCode.SUCCESSFUL, send(token, HttpMethod.POST, "/selector/batchEnabled", batch(false, selectorId)));
        assertEquals(CommonErrorCode.SUCCESSFUL, send(token, HttpMethod.POST, "/rule", rule(selectorId, "divide-r")));
        final String ruleId = ruleId("divide-r");
        assertEquals(CommonErrorCode.SUCCESSFUL, send(token, HttpMethod.PUT, "/rule/" + ruleId, rule(selectorId, "divide-r")));
        assertEquals(CommonErrorCode.SUCCESSFUL, send(token, HttpMethod.POST, "/rule/batchEnabled", batch(false, ruleId)));
        assertEquals(CommonErrorCode.SUCCESSFUL, send(token, HttpMethod.DELETE, "/rule/batch", batch(null, ruleId)));
        assertEquals(0, count("rule", "divide-r"));

        // the same writes on another plugin are refused and change nothing
        assertEquals(CommonErrorCode.SUCCESSFUL, send(adminToken, HttpMethod.POST, "/selector", selector(grpcPluginId, "grpc-s")));
        final String grpcSelectorId = selectorId("grpc-s");
        assertEquals(CommonErrorCode.TOKEN_NO_PERMISSION, send(token, HttpMethod.POST, "/selector", selector(grpcPluginId, "grpc-denied")));
        assertEquals(CommonErrorCode.TOKEN_NO_PERMISSION, send(token, HttpMethod.PUT, "/selector/" + grpcSelectorId, selector(grpcPluginId, "grpc-s")));
        assertEquals(CommonErrorCode.TOKEN_NO_PERMISSION, send(token, HttpMethod.POST, "/selector/batchEnabled", batch(false, grpcSelectorId)));
        assertEquals(CommonErrorCode.TOKEN_NO_PERMISSION, send(token, HttpMethod.POST, "/rule", rule(grpcSelectorId, "grpc-denied")));
        // a batch is refused as a whole when one of its items belongs to another plugin
        assertEquals(CommonErrorCode.TOKEN_NO_PERMISSION, send(token, HttpMethod.DELETE, "/selector/batch", batch(null, selectorId, grpcSelectorId)));
        assertEquals(0, count("selector", "grpc-denied"));
        assertEquals(0, count("rule", "grpc-denied"));
        assertEquals(1, count("selector", "divide-s"));
        assertEquals(1, count("selector", "grpc-s"));
        assertEquals(1, jdbcTemplate.queryForObject("SELECT COUNT(*) FROM selector WHERE id = ? AND enabled = 1", Integer.class, grpcSelectorId));

        assertEquals(CommonErrorCode.SUCCESSFUL, send(token, HttpMethod.DELETE, "/selector/batch", batch(null, selectorId)));
        assertEquals(0, count("selector", "divide-s"));
    }

    @Test
    public void testGlobalGrantsAllowWritesOnEveryPlugin() throws Exception {
        final String token = loginWithGrants("global", "system:plugin:edit", "system:plugin:disable", "system:plugin:delete");

        for (String pluginId : new String[] {dividePluginId, grpcPluginId}) {
            assertEquals(CommonErrorCode.SUCCESSFUL, send(token, HttpMethod.POST, "/selector", selector(pluginId, "global-s")));
            final String selectorId = selectorId("global-s");
            assertEquals(CommonErrorCode.SUCCESSFUL, send(token, HttpMethod.POST, "/rule", rule(selectorId, "global-r")));
            assertEquals(CommonErrorCode.SUCCESSFUL, send(token, HttpMethod.POST, "/selector/batchEnabled", batch(false, selectorId)));
            assertEquals(CommonErrorCode.SUCCESSFUL, send(token, HttpMethod.DELETE, "/selector/batch", batch(null, selectorId)));
            assertEquals(0, count("selector", "global-s"));
        }
    }

    @Test
    public void testQueryGrantsDoNotAllowWrites() throws Exception {
        final String token = loginWithGrants("readonly", "plugin:divideSelector:query", "plugin:divideRule:query");
        final String adminToken = login("admin");
        assertEquals(CommonErrorCode.SUCCESSFUL, send(adminToken, HttpMethod.POST, "/selector", selector(dividePluginId, "readonly-s")));
        final String selectorId = selectorId("readonly-s");

        assertEquals(CommonErrorCode.TOKEN_NO_PERMISSION, send(token, HttpMethod.POST, "/selector", selector(dividePluginId, "readonly-denied")));
        assertEquals(CommonErrorCode.TOKEN_NO_PERMISSION, send(token, HttpMethod.PUT, "/selector/" + selectorId, selector(dividePluginId, "readonly-s")));
        assertEquals(CommonErrorCode.TOKEN_NO_PERMISSION, send(token, HttpMethod.POST, "/rule", rule(selectorId, "readonly-denied")));
        assertEquals(CommonErrorCode.TOKEN_NO_PERMISSION, send(token, HttpMethod.DELETE, "/selector/batch", batch(null, selectorId)));
        assertEquals(0, count("selector", "readonly-denied"));
        assertEquals(0, count("rule", "readonly-denied"));
        assertEquals(1, count("selector", "readonly-s"));
    }

    private String[] pluginGrants(final String pluginName) {
        final String[] grants = new String[WRITE_ACTIONS.length];
        for (int i = 0; i < WRITE_ACTIONS.length; i++) {
            grants[i] = "plugin:" + pluginName + WRITE_ACTIONS[i];
        }
        return grants;
    }

    private String loginWithGrants(final String name, final String... grants) throws Exception {
        final String id = PREFIX + name;
        jdbcTemplate.update("INSERT INTO role (id, role_name, description) VALUES (?, ?, ?)", id, id, id);
        jdbcTemplate.update("INSERT INTO dashboard_user (id, user_name, password, role, enabled)"
                + " SELECT ?, ?, password, 0, 1 FROM dashboard_user WHERE user_name = 'admin'", id, id);
        jdbcTemplate.update("INSERT INTO user_role (id, user_id, role_id) VALUES (?, ?, ?)", id, id, id);
        for (String grant : grants) {
            // the resource rows are the ones the application created for the plugin
            assertEquals(1, jdbcTemplate.update("INSERT INTO permission (id, object_id, resource_id) SELECT ?, ?, id FROM resource WHERE perms = ?",
                    id + "-" + sequence++, id, grant), grant);
        }
        return login(id);
    }

    private String login(final String userName) throws Exception {
        final String body = mockMvc.perform(MockMvcRequestBuilders.post("/platform/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"userName\":\"" + userName + "\",\"password\":\"" + PASSWORD + "\"}"))
                .andReturn().getResponse().getContentAsString();
        return JsonPath.read(body, "$.data.token");
    }

    private int send(final String token, final HttpMethod method, final String path, final String body) throws Exception {
        final String response = mockMvc.perform(MockMvcRequestBuilders.request(method, path)
                        .header(Constants.X_ACCESS_TOKEN, token)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andReturn().getResponse().getContentAsString();
        return JsonPath.read(response, "$.code");
    }

    private String selector(final String pluginId, final String name) {
        return "{\"pluginId\":\"" + pluginId + "\",\"name\":\"" + PREFIX + name + "\",\"type\":1,\"matchMode\":0,\"sort\":1,"
                + "\"enabled\":true,\"logged\":true,\"continued\":true,\"matchRestful\":false,\"handle\":\"\","
                + "\"namespaceId\":\"" + NAMESPACE + "\",\"selectorConditions\":[" + CONDITION + "]}";
    }

    private String rule(final String selectorId, final String name) {
        return "{\"selectorId\":\"" + selectorId + "\",\"name\":\"" + PREFIX + name + "\",\"matchMode\":0,\"sort\":1,"
                + "\"enabled\":true,\"logged\":true,\"matchRestful\":false,\"handle\":\"\","
                + "\"namespaceId\":\"" + NAMESPACE + "\",\"ruleConditions\":[" + CONDITION + "]}";
    }

    private String batch(final Boolean enabled, final String... ids) {
        return "{\"ids\":[\"" + String.join("\",\"", ids) + "\"],"
                + (Objects.isNull(enabled) ? "" : "\"enabled\":" + enabled + ",")
                + "\"namespaceId\":\"" + NAMESPACE + "\"}";
    }

    private String selectorId(final String name) {
        return jdbcTemplate.queryForObject("SELECT id FROM selector WHERE selector_name = ?", String.class, PREFIX + name);
    }

    private String ruleId(final String name) {
        return jdbcTemplate.queryForObject("SELECT id FROM rule WHERE rule_name = ?", String.class, PREFIX + name);
    }

    private int count(final String table, final String name) {
        return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM " + table + " WHERE " + table + "_name = ?", Integer.class, PREFIX + name);
    }
}
