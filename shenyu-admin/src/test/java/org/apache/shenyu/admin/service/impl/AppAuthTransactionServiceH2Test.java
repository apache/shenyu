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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AppAuthRequestMapper;
import org.apache.shenyu.admin.mapper.AuthParamMapper;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.model.dto.AppAuthApplyRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthReplacePathsRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthResultQueryDTO;
import org.apache.shenyu.admin.model.dto.AppAuthRevokeRequestDTO;
import org.apache.shenyu.admin.model.vo.AppAuthResultVO;
import org.apache.ibatis.session.SqlSessionFactory;
import org.h2.jdbcx.JdbcDataSource;
import org.h2.tools.RunScript;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mybatis.spring.SqlSessionFactoryBean;
import org.mybatis.spring.mapper.MapperFactoryBean;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import javax.sql.DataSource;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Lightweight H2 coverage for the apply/result/revoke/replacePaths transaction service:
 * no external MySQL and no system-property gate, so it runs in the default surefire pass
 * (the deep MySQL-fixture suites stay in AppAuthApplyTest and friends).
 * Covers first apply, same-parameter replay, different-parameter rejection, replacePaths
 * null/empty/whole-replace semantics, the result tri-state and the revoke tombstone.
 */
class AppAuthTransactionServiceH2Test {

    private static AnnotationConfigApplicationContext context;

    private static AppAuthTransactionService txService;

    /** Recorded data-changed events (visible after commit). */
    private static final List<Object> PUBLISHED_EVENTS = new CopyOnWriteArrayList<>();

    @BeforeAll
    static void setUp() {
        context = new AnnotationConfigApplicationContext(H2TxConfig.class);
        txService = context.getBean(AppAuthTransactionService.class);
    }

    @AfterAll
    static void tearDown() {
        if (Objects.nonNull(context)) {
            context.close();
        }
    }

    private static AppAuthApplyRequestDTO applyRequest(final String requestId) {
        final AppAuthApplyRequestDTO dto = new AppAuthApplyRequestDTO();
        dto.setNamespaceId("ns-h2");
        dto.setRequestId(requestId);
        dto.setAppName("h2-app");
        dto.setUserId("u1");
        dto.setPathList(List.of("/a", "/b"));
        return dto;
    }

    private static AppAuthResultQueryDTO query(final String requestId) {
        final AppAuthResultQueryDTO q = new AppAuthResultQueryDTO();
        q.setNamespaceId("ns-h2");
        q.setRequestId(requestId);
        return q;
    }

    @Test
    void applyReplayReplacePathsRevokeLifecycle() {
        // first apply: CREATED with credentials, enabled=false/open=true fixed
        final AppAuthResultVO first = txService.apply("caller", applyRequest("r1"), "hash-v2");
        assertEquals("CREATED", first.getState());
        assertNotNull(first.getAppKey());
        assertNotNull(first.getAppSecret());
        assertEquals(Boolean.FALSE, first.getEnabled());
        assertEquals(Boolean.TRUE, first.getOpen());

        // same-parameter replay returns the same credentials
        final AppAuthResultVO replay = txService.apply("caller", applyRequest("r1"), "hash-v2");
        assertEquals(first.getId(), replay.getId());
        assertEquals(first.getAppKey(), replay.getAppKey());

        // same requestId with different parameters is rejected (the facade supplies the digest, so different
        // parameters arrive here as a different digest for the same requestId)
        final AppAuthApplyRequestDTO different = applyRequest("r1");
        different.setAppName("other-app");
        assertThrows(Exception.class, () -> txService.apply("caller", different, "hash-different"));

        // replacePaths: null is rejected, empty clears, non-empty replaces as a whole (de-duplicated)
        final AppAuthReplacePathsRequestDTO replace = new AppAuthReplacePathsRequestDTO();
        replace.setNamespaceId("ns-h2");
        replace.setAuthId(first.getId());
        replace.setPathList(null);
        assertThrows(Exception.class, () -> txService.replacePaths("caller", replace));
        replace.setPathList(List.of());
        assertEquals(0, txService.replacePaths("caller", replace).getPathList().size());
        replace.setPathList(List.of("/x", "/x", "/y"));
        final List<String> replaced = txService.replacePaths("caller", replace).getPathList();
        assertEquals(2, replaced.size());
        assertTrue(replaced.contains("/x") && replaced.contains("/y"));

        // result tri-state: NOT_FOUND for unknown keys, CREATED (with credentials) for r1
        assertEquals("NOT_FOUND", txService.result("caller", query("r-none")).getState());
        final AppAuthResultVO created = txService.result("caller", query("r1"));
        assertEquals("CREATED", created.getState());
        assertEquals(first.getAppKey(), created.getAppKey());

        // revoke is idempotent and installs a tombstone that rejects late applies
        final AppAuthRevokeRequestDTO revoke = new AppAuthRevokeRequestDTO();
        revoke.setNamespaceId("ns-h2");
        revoke.setRequestId("r1");
        revoke.setReason("done");
        assertEquals("REVOKED", txService.revoke("caller", revoke).getState());
        assertEquals("REVOKED", txService.revoke("caller", revoke).getState());
        assertEquals("REVOKED", txService.result("caller", query("r1")).getState());
        assertNull(txService.result("caller", query("r1")).getAppKey());
        assertThrows(Exception.class, () -> txService.apply("caller", applyRequest("r1"), "hash-v2"));

        // commit-time sync events fired for create/update/delete
        assertTrue(PUBLISHED_EVENTS.stream().allMatch(DataChangedEvent.class::isInstance));
        assertTrue(PUBLISHED_EVENTS.size() >= 3);
    }

    /** H2 in-memory MySQL-mode database initialized from the maintained H2 schema (includes app_auth_request). */
    @Configuration
    @EnableTransactionManagement
    static class H2TxConfig {

        @Bean
        public DataSource dataSource() throws Exception {
            final JdbcDataSource ds = new JdbcDataSource();
            ds.setURL("jdbc:h2:mem:appauthtx;MODE=MySQL;DATABASE_TO_LOWER=TRUE;DB_CLOSE_DELAY=-1");
            try (java.sql.Connection connection = ds.getConnection()) {
                RunScript.execute(connection, java.nio.file.Files.newBufferedReader(
                        Paths.get("src/main/resources/sql-script/h2/schema.sql"), StandardCharsets.UTF_8));
            }
            return ds;
        }

        @Bean
        public DataSourceTransactionManager transactionManager(final DataSource dataSource) {
            return new DataSourceTransactionManager(dataSource);
        }

        @Bean
        public SqlSessionFactory sqlSessionFactory(final DataSource dataSource) throws Exception {
            final SqlSessionFactoryBean factoryBean = new SqlSessionFactoryBean();
            factoryBean.setDataSource(dataSource);
            factoryBean.setTypeAliasesPackage("org.apache.shenyu.admin.model.entity");
            factoryBean.setMapperLocations(
                    new ClassPathResource("mappers/app-auth-request-sqlmap.xml"),
                    new ClassPathResource("mappers/app-auth-sqlmap.xml"),
                    new ClassPathResource("mappers/auth-param-sqlmap.xml"),
                    new ClassPathResource("mappers/auth-path-sqlmap.xml"));
            return factoryBean.getObject();
        }

        @Bean
        public MapperFactoryBean<AppAuthRequestMapper> appAuthRequestMapper(final SqlSessionFactory factory) {
            final MapperFactoryBean<AppAuthRequestMapper> bean = new MapperFactoryBean<>(AppAuthRequestMapper.class);
            bean.setSqlSessionFactory(factory);
            return bean;
        }

        @Bean
        public MapperFactoryBean<AppAuthMapper> appAuthMapper(final SqlSessionFactory factory) {
            final MapperFactoryBean<AppAuthMapper> bean = new MapperFactoryBean<>(AppAuthMapper.class);
            bean.setSqlSessionFactory(factory);
            return bean;
        }

        @Bean
        public MapperFactoryBean<AuthParamMapper> authParamMapper(final SqlSessionFactory factory) {
            final MapperFactoryBean<AuthParamMapper> bean = new MapperFactoryBean<>(AuthParamMapper.class);
            bean.setSqlSessionFactory(factory);
            return bean;
        }

        @Bean
        public MapperFactoryBean<AuthPathMapper> authPathMapper(final SqlSessionFactory factory) {
            final MapperFactoryBean<AuthPathMapper> bean = new MapperFactoryBean<>(AuthPathMapper.class);
            bean.setSqlSessionFactory(factory);
            return bean;
        }

        @Bean
        public AppAuthTransactionService appAuthTransactionService(final AppAuthRequestMapper requestMapper,
                                                                    final AppAuthMapper appAuthMapper,
                                                                    final AuthParamMapper authParamMapper,
                                                                    final AuthPathMapper authPathMapper) {
            // recorder passed directly to the constructor (container injection of ApplicationEventPublisher
            // resolves to the ApplicationContext, whose multicaster has no listeners in this bare context)
            final ApplicationEventPublisher recorder = PUBLISHED_EVENTS::add;
            return new AppAuthTransactionService(requestMapper, appAuthMapper, authParamMapper,
                    authPathMapper, recorder);
        }
    }
}
