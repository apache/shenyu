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

import org.apache.ibatis.builder.xml.XMLMapperBuilder;
import org.apache.ibatis.mapping.Environment;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import org.apache.ibatis.transaction.jdbc.JdbcTransactionFactory;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.service.impl.NamespaceServiceImpl;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.constant.AdminConstants;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;

import java.io.InputStream;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Tests namespace-list filtering against the SQL mapper for both user scopes.
 */
class NamespaceListTest {

    private JdbcTemplate jdbc;

    private SqlSession sqlSession;

    private NamespaceUserService namespaceUserService;

    private NamespaceService namespaceService;

    @BeforeEach
    void setUp() throws Exception {
        DriverManagerDataSource dataSource = new DriverManagerDataSource(
                "jdbc:h2:mem:namespace-list-" + UUID.randomUUID() + ";DB_CLOSE_DELAY=-1;MODE=MySQL", "sa", "");
        jdbc = new JdbcTemplate(dataSource);
        new ResourceDatabasePopulator(new ClassPathResource("sql-script/h2/schema.sql")).execute(dataSource);
        jdbc.update("DELETE FROM namespace");
        jdbc.update("INSERT INTO namespace (id, namespace_id, name) VALUES ('1', 'payments-prod', 'payments-production')");
        jdbc.update("INSERT INTO namespace (id, namespace_id, name) VALUES ('2', 'payments-test', 'payments-testing')");
        jdbc.update("INSERT INTO namespace (id, namespace_id, name) VALUES ('3', 'orders-prod', 'orders-production')");

        Configuration configuration = new Configuration(new Environment("test", new JdbcTransactionFactory(), dataSource));
        configuration.setMapUnderscoreToCamelCase(true);
        try (InputStream input = new ClassPathResource("mappers/namespace-sqlmap.xml").getInputStream()) {
            new XMLMapperBuilder(input, configuration, "namespace", configuration.getSqlFragments()).parse();
        }
        sqlSession = new SqlSessionFactoryBuilder().build(configuration).openSession();
        namespaceUserService = mock(NamespaceUserService.class);
        namespaceService = new NamespaceServiceImpl(sqlSession.getMapper(NamespaceMapper.class), namespaceUserService,
                null, null, null, null, null, null, null, null);
        SessionUtil.setLocalVisitor(UserInfo.builder().userId("admin").userName(AdminConstants.ADMIN_NAME).build());
    }

    @AfterEach
    void tearDown() {
        SessionUtil.clean();
        if (Objects.nonNull(sqlSession)) {
            sqlSession.close();
        }
        if (Objects.nonNull(jdbc)) {
            jdbc.execute("SHUTDOWN");
        }
    }

    @Test
    void administratorCanFilterByPartialName() {
        assertEquals(Set.of("payments-prod", "payments-test"), namespaceIds(namespaceService.list("payments")));
        verifyNoInteractions(namespaceUserService);
    }

    @ParameterizedTest
    @ValueSource(strings = {"pay%nts", "pay_ents"})
    void bothUserScopesPreserveSqlLikeWildcardMatching(final String name) {
        // Preserve the existing SQL LIKE wildcard behavior in both user scopes.
        assertEquals(Set.of("payments-prod", "payments-test"), namespaceIds(namespaceService.list(name)));
        verifyNoInteractions(namespaceUserService);

        SessionUtil.setLocalVisitor(UserInfo.builder().userId("user").userName("regular-user").build());
        when(namespaceUserService.listNamespaceIdByUserId("user")).thenReturn(List.of("payments-prod", "orders-prod"));

        assertEquals(Set.of("payments-prod"), namespaceIds(namespaceService.list(name)));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void administratorCanListAllNamespacesWithoutAName(final String name) {
        assertEquals(Set.of("payments-prod", "payments-test", "orders-prod"), namespaceIds(namespaceService.list(name)));
        verifyNoInteractions(namespaceUserService);
    }

    @Test
    void administratorGetsAnEmptyListWhenNoNameMatches() {
        assertTrue(namespaceService.list("missing").isEmpty());
    }

    @Test
    void regularUserCanFilterOnlyAuthorizedNamespaces() {
        SessionUtil.setLocalVisitor(UserInfo.builder().userId("user").userName("regular-user").build());
        when(namespaceUserService.listNamespaceIdByUserId("user")).thenReturn(List.of("payments-prod", "orders-prod"));

        assertEquals(Set.of("payments-prod"), namespaceIds(namespaceService.list("payments")));
        assertTrue(namespaceService.list("testing").isEmpty());
    }

    @ParameterizedTest
    @NullAndEmptySource
    void regularUserCanListAuthorizedNamespacesWithoutAName(final String name) {
        SessionUtil.setLocalVisitor(UserInfo.builder().userId("user").userName("regular-user").build());
        when(namespaceUserService.listNamespaceIdByUserId("user")).thenReturn(List.of("payments-prod", "orders-prod"));

        assertEquals(Set.of("payments-prod", "orders-prod"), namespaceIds(namespaceService.list(name)));
    }

    @Test
    void regularUserWithoutNamespacesGetsAnEmptyList() {
        SessionUtil.setLocalVisitor(UserInfo.builder().userId("user").userName("regular-user").build());
        when(namespaceUserService.listNamespaceIdByUserId("user")).thenReturn(List.of());

        assertTrue(namespaceService.list("payments").isEmpty());
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = "payments")
    void regularUserWithNullNamespaceMappingsGetsAnEmptyList(final String name) {
        SessionUtil.setLocalVisitor(UserInfo.builder().userId("user").userName("regular-user").build());
        when(namespaceUserService.listNamespaceIdByUserId("user")).thenReturn(null);

        assertTrue(namespaceService.list(name).isEmpty());
    }

    private Set<String> namespaceIds(final List<NamespaceVO> namespaces) {
        return namespaces.stream().map(NamespaceVO::getNamespaceId).collect(Collectors.toSet());
    }
}
