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

package org.apache.shenyu.authtest;

import org.apache.ibatis.datasource.unpooled.UnpooledDataSource;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.service.impl.AppAuthServiceImpl;
import org.apache.shenyu.admin.service.impl.AppAuthMySqlFixture;
import org.apache.shenyu.admin.service.impl.AppAuthTransactionService;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AuthParamMapper;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.mapper.AppAuthRequestMapper;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Spring transactional test support on the isolated database: real DataSourceTransactionManager + MyBatis mappers,
 * reused by the lifecycle transaction service tests (AppAuthApplyTest etc.).
 */
public final class AppAuthTestSupport {

    private AppAuthTestSupport() {
    }

    /**
     * Create/recreate the lifecycle test tables (destructive operations run on the isolated database only).
     *
     * @throws Exception when table creation fails
     */
    public static void recreateTables() throws Exception {
        AppAuthMySqlFixture.requireExplicitTestConfig();
        try (Connection connection = AppAuthMySqlFixture.openConnection();
             Statement statement = connection.createStatement()) {
            AppAuthMySqlFixture.verifyTestDatabaseIdentity(connection);
            statement.execute("DROP TABLE IF EXISTS app_auth_request");
            statement.execute("DROP TABLE IF EXISTS auth_path");
            statement.execute("DROP TABLE IF EXISTS auth_param");
            statement.execute("DROP TABLE IF EXISTS app_auth");
            for (final String ddl : schemaDdls()) {
                statement.execute(ddl);
            }
        }
    }

    private static List<String> schemaDdls() throws Exception {
        final Path schema = Paths.get("../db/init/mysql/schema.sql");
        if (!Files.exists(schema)) {
            throw new IllegalStateException("schema.sql not found: " + schema);
        }
        final String sql = Files.readString(schema, StandardCharsets.UTF_8);
        final List<String> ddls = new java.util.ArrayList<>();
        for (final String table : new String[] {"app_auth", "auth_param", "auth_path"}) {
            final int start = sql.indexOf("CREATE TABLE `" + table + "`");
            if (start < 0) {
                throw new IllegalStateException("schema.sql is missing table " + table);
            }
            final int end = sql.indexOf(';', start);
            ddls.add(sql.substring(start, end));
        }
        ddls.add("CREATE TABLE app_auth_request ("
                + "id VARCHAR(128) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,"
                + "caller_key VARCHAR(128) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,"
                + "namespace_id VARCHAR(64) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,"
                + "request_id VARCHAR(64) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,"
                + "request_hash VARCHAR(64) CHARACTER SET ascii COLLATE ascii_bin NULL,"
                + "auth_id VARCHAR(255) CHARACTER SET ascii COLLATE ascii_bin NULL,"
                + "state VARCHAR(16) CHARACTER SET ascii NOT NULL,"
                + "revoked_reason VARCHAR(500) NULL,"
                + "date_created DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),"
                + "date_updated DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3),"
                + "PRIMARY KEY (id), UNIQUE KEY uk_request (caller_key, namespace_id, request_id),"
                + "KEY idx_auth (auth_id)) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci");
        return ddls;
    }

    /**
     * Open the transactional context (the caller must close it).
     *
     * @return the Spring context
     */
    public static AnnotationConfigApplicationContext openContext() {
        final AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();
        context.register(TxConfig.class);
        context.refresh();
        return context;
    }

    /**
     * Open a bare SqlSession for direct table assertions (independent of the transactional context).
     *
     * @param factory factory from context
     * @return the SqlSession (the caller must close it)
     */
    public static SqlSession openInspectionSession(final SqlSessionFactory factory) {
        return factory.openSession(true);
    }

    /**
     * Count query for assertions.
     *
     * @param factory factory
     * @param table table name
     * @param whereSql where clause including the WHERE keyword, or empty
     * @return count
     * @throws SQLException when the query fails
     */
    public static int countRows(final SqlSessionFactory factory, final String table, final String whereSql)
            throws SQLException {
        try (SqlSession session = openInspectionSession(factory);
             Connection connection = session.getConnection();
             Statement statement = connection.createStatement();
             java.sql.ResultSet rs = statement.executeQuery(
                     "SELECT COUNT(*) FROM " + table + (Objects.isNull(whereSql) || whereSql.isEmpty() ? "" : " " + whereSql))) {
            rs.next();
            return rs.getInt(1);
        }
    }

    /**
     * Spring transactional context (real transaction manager, four mappers, the service under test, an event recorder).
     */
    @Configuration
    @EnableTransactionManagement
    public static class TxConfig {

        /** Recorded data-changed events (thread safe; visible after afterCommit). */
        public static final List<Object> PUBLISHED_EVENTS = new CopyOnWriteArrayList<>();

        @Bean
        public DataSource dataSource() {
            return new UnpooledDataSource("com.mysql.cj.jdbc.Driver",
                    System.getProperty("shenyu.auth.test.jdbcUrl"),
                    System.getProperty("shenyu.auth.test.username"),
                    System.getProperty("shenyu.auth.test.password"));
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
                    new ClassPathResource("mappers/flm-app-auth-request-sqlmap.xml"),
                    new ClassPathResource("mappers/app-auth-sqlmap.xml"),
                    new ClassPathResource("mappers/auth-param-sqlmap.xml"),
                    new ClassPathResource("mappers/auth-path-sqlmap.xml"));
            return factoryBean.getObject();
        }

        @Bean
        public MapperFactoryBean<AppAuthRequestMapper> flmAppAuthRequestMapper(final SqlSessionFactory factory) {
            final MapperFactoryBean<AppAuthRequestMapper> bean =
                    new MapperFactoryBean<>(AppAuthRequestMapper.class);
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
        public AppAuthServiceImpl appAuthService(final AppAuthMapper appAuthMapper,
                                                 final AuthParamMapper authParamMapper,
                                                 final AuthPathMapper authPathMapper,
                                                 final AppAuthRequestMapper requestMapper) {
            final ApplicationEventPublisher recorder = event -> {
                if (event instanceof DataChangedEvent) {
                    PUBLISHED_EVENTS.add(event);
                }
            };
            return new AppAuthServiceImpl(appAuthMapper, recorder, authParamMapper, authPathMapper, requestMapper);
        }

        @Bean
        public AppAuthTransactionService flmAppAuthTransactionService(
                final AppAuthRequestMapper requestMapper, final AppAuthMapper appAuthMapper,
                final AuthParamMapper authParamMapper, final AuthPathMapper authPathMapper) {
            // explicit inline event recorder: injecting an ApplicationEventPublisher by type would be hijacked by the container itself
            final ApplicationEventPublisher recorder = event -> {
                if (event instanceof DataChangedEvent) {
                    PUBLISHED_EVENTS.add(event);
                }
            };
            return new AppAuthTransactionService(requestMapper, appAuthMapper,
                    authParamMapper, authPathMapper, recorder);
        }
    }

}
