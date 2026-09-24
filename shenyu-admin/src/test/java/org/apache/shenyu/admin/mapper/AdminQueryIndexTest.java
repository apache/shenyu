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

package org.apache.shenyu.admin.mapper;

import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AdminQueryIndexTest extends AbstractSpringIntegrationTest {

    @Resource
    private DataSource dataSource;

    @ParameterizedTest
    @MethodSource("indexes")
    void testLookupIndexColumnOrder(final String table, final String index, final String columns) throws Exception {
        List<String> actual = new ArrayList<>();
        try (Connection connection = dataSource.getConnection();
                ResultSet indexes = connection.getMetaData().getIndexInfo(null, null,
                        connection.getMetaData().storesUpperCaseIdentifiers() ? table.toUpperCase(Locale.ROOT) : table, false, false)) {
            while (indexes.next()) {
                if (index.equalsIgnoreCase(indexes.getString("INDEX_NAME"))) {
                    actual.add(indexes.getString("COLUMN_NAME").toLowerCase(Locale.ROOT));
                }
            }
        }
        assertEquals(columns, String.join(",", actual));
    }

    private static Stream<Arguments> indexes() {
        return Stream.of(
                Arguments.of("selector", "idx_selector_ns_plugin_name", "namespace_id,plugin_id,selector_name"),
                Arguments.of("rule", "idx_rule_ns_selector_name", "namespace_id,selector_id,rule_name"),
                Arguments.of("meta_data", "idx_metadata_path_ns", "path,namespace_id"),
                Arguments.of("meta_data", "idx_metadata_ns_service", "namespace_id,service_name"),
                Arguments.of("meta_data", "idx_metadata_ns_app", "namespace_id,app_name"),
                Arguments.of("app_auth", "idx_app_auth_ns_key", "namespace_id,app_key"),
                Arguments.of("auth_path", "idx_auth_path_auth", "auth_id"),
                Arguments.of("auth_param", "idx_auth_param_auth", "auth_id"),
                Arguments.of("permission", "idx_permission_object_resource", "object_id,resource_id"),
                Arguments.of("data_permission", "idx_data_permission_user_type", "user_id,data_type,data_id"),
                Arguments.of("tag_relation", "idx_tag_relation_api_tag", "api_id,tag_id"),
                Arguments.of("tag_relation", "idx_tag_relation_tag_api", "tag_id,api_id"),
                Arguments.of("api", "idx_api_path_method_rpc", "api_path,http_method,rpc_type"),
                Arguments.of("api", "idx_api_context", "context_path"),
                Arguments.of("api", "idx_api_state_created", "state,date_created"),
                Arguments.of("api", "idx_api_created", "date_created"),
                Arguments.of("api_rule_relation", "idx_api_rule_api_rule", "api_id,rule_id"),
                Arguments.of("namespace_plugin_rel", "idx_ns_plugin_ns_plugin", "namespace_id,plugin_id,enabled"),
                Arguments.of("discovery_rel", "idx_discovery_rel_proxy", "proxy_selector_id"),
                Arguments.of("discovery_rel", "idx_discovery_rel_selector", "selector_id"),
                Arguments.of("discovery_rel", "idx_discovery_rel_handler", "discovery_handler_id"),
                Arguments.of("discovery_handler", "idx_discovery_handler_discovery", "discovery_id"),
                Arguments.of("discovery", "idx_discovery_ns_plugin", "namespace_id,plugin_name"),
                Arguments.of("proxy_selector", "idx_proxy_selector_ns", "namespace_id"),
                Arguments.of("operation_record_log", "idx_operation_log_time", "operation_time"),
                Arguments.of("operation_record_log", "idx_operation_log_operator_time", "operator,operation_time"),
                Arguments.of("instance_info", "idx_instance_ns_ip", "namespace_id,instance_ip"),
                Arguments.of("mock_request_record", "idx_mock_record_api", "api_id"),
                Arguments.of("namespace_user_rel", "idx_namespace_user_ns_user", "namespace_id,user_id"),
                Arguments.of("namespace_user_rel", "idx_namespace_user_user_ns", "user_id,namespace_id"),
                Arguments.of("user_role", "idx_user_role_user_role", "user_id,role_id"),
                Arguments.of("resource", "idx_resource_parent", "parent_id"));
    }
}
