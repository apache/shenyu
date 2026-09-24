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

import com.github.pagehelper.PageHelper;
import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.AppAuthQuery;
import org.apache.shenyu.admin.model.query.DashboardUserQuery;
import org.apache.shenyu.admin.model.query.InstanceQuery;
import org.apache.shenyu.admin.model.query.MockRequestRecordQuery;
import org.apache.shenyu.admin.model.query.NamespaceQuery;
import org.apache.shenyu.admin.model.query.RegistryQuery;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.annotation.Transactional;

import java.util.function.IntFunction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mockStatic;

@Transactional
class AdminListPaginationTest extends AbstractSpringIntegrationTest {

    @Resource
    private JdbcTemplate jdbcTemplate;

    @Resource
    private AppAuthService appAuthService;

    @Resource
    private DashboardUserService dashboardUserService;

    @Resource
    private RegistryService registryService;

    @Resource
    private InstanceInfoService instanceInfoService;

    @Resource
    private MockRequestRecordService mockRequestRecordService;

    @Resource
    private NamespaceService namespaceService;

    @Test
    void testListsUseSqlPagination() {
        for (int index = 0; index < 2; index++) {
            String id = "page-test-" + index;
            jdbcTemplate.update("INSERT INTO app_auth (id, app_key, app_secret, open, enabled, namespace_id) VALUES (?, ?, 'secret', 1, 1, 'page-test')", id, id);
            jdbcTemplate.update("INSERT INTO dashboard_user (id, user_name, role, enabled) VALUES (?, ?, 1, 1)", id, id);
            jdbcTemplate.update("INSERT INTO registry_config (id, registry_id, protocol, address, namespace) VALUES (?, ?, 'http', 'localhost', 'page-test')", id, id);
            jdbcTemplate.update("INSERT INTO instance_info (id, namespace_id, instance_ip, instance_port, instance_type, instance_info, instance_state)"
                    + " VALUES (?, 'page-test', '127.0.0.1', '9195', 'gateway', '{}', 1)", id);
            jdbcTemplate.update("INSERT INTO mock_request_record (id, api_id, host, port, url) VALUES (?, 'page-test', 'localhost', 80, '/')", id);
        }
        assertPages(page -> {
            AppAuthQuery query = new AppAuthQuery();
            query.setNamespaceId("page-test");
            query.setPageParameter(new PageParameter(page, 1));
            return appAuthService.listByPage(query);
        });
        assertPages(page -> {
            DashboardUserQuery query = new DashboardUserQuery();
            query.setUserName("page-test-");
            query.setPageParameter(new PageParameter(page, 1));
            return dashboardUserService.listByPage(query);
        });
        assertPages(page -> {
            RegistryQuery query = new RegistryQuery();
            query.setNamespace("page-test");
            query.setPageParameter(new PageParameter(page, 1));
            return registryService.listByPage(query);
        });
        assertPages(page -> instanceInfoService.listByPage(new InstanceQuery(new PageParameter(page, 1), null, null, null, "page-test")));
        assertPages(page -> {
            MockRequestRecordQuery query = new MockRequestRecordQuery();
            query.setApiId("page-test");
            query.setPageParameter(new PageParameter(page, 1));
            return mockRequestRecordService.listByPage(query);
        });
    }

    @Test
    void testNamespacePermissionLookupDoesNotConsumePage() {
        for (int index = 0; index < 2; index++) {
            String id = "page-test-" + index;
            jdbcTemplate.update("INSERT INTO namespace (id, namespace_id, name) VALUES (?, ?, ?)", id, id, id);
            jdbcTemplate.update("INSERT INTO namespace_user_rel (id, namespace_id, user_id) VALUES (?, ?, 'page-user')", id, id);
        }
        try (MockedStatic<SessionUtil> session = mockStatic(SessionUtil.class)) {
            session.when(SessionUtil::isAdmin).thenReturn(false);
            session.when(SessionUtil::visitorId).thenReturn("page-user");
            assertPages(page -> {
                NamespaceQuery query = new NamespaceQuery();
                query.setName("page-test-");
                query.setPageParameter(new PageParameter(page, 1));
                return namespaceService.listByPage(query);
            });
            assertNull(PageHelper.getLocalPage());
        }
    }

    private void assertPages(final IntFunction<CommonPager<?>> query) {
        for (int page = 1; page <= 3; page++) {
            CommonPager<?> result = query.apply(page);
            assertEquals(2, result.getPage().getTotalCount());
            assertEquals(page <= 2 ? 1 : 0, result.getDataList().size());
            assertNull(PageHelper.getLocalPage());
        }
    }
}

