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

import org.apache.ibatis.builder.xml.XMLMapperBuilder;
import org.apache.ibatis.mapping.Environment;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import org.apache.ibatis.transaction.jdbc.JdbcTransactionFactory;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.fencing.RegistrationFencingGuard;
import org.apache.shenyu.admin.service.impl.DiscoveryServiceImpl;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import javax.sql.DataSource;
import java.io.InputStream;
import java.util.Objects;
import java.util.UUID;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

class SelectorDiscoveryLookupTest {

    private JdbcTemplate jdbc;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() throws Exception {
        DriverManagerDataSource dataSource = new DriverManagerDataSource(
                "jdbc:h2:mem:selector-discovery-" + UUID.randomUUID() + ";DB_CLOSE_DELAY=-1;MODE=MySQL", "sa", "");
        jdbc = new JdbcTemplate(dataSource);
        new ResourceDatabasePopulator(new ClassPathResource("sql-script/h2/schema.sql")).execute(dataSource);
        DiscoveryMapper discoveryMapper = new SqlSessionTemplate(sessionFactory(dataSource, "h2")).getMapper(DiscoveryMapper.class);
        DiscoveryServiceImpl service = new DiscoveryServiceImpl(discoveryMapper,
                mock(ProxySelectorMapper.class), mock(DiscoveryRelMapper.class), mock(DiscoveryHandlerMapper.class),
                mock(SelectorService.class), mock(SelectorMapper.class), mock(DiscoveryProcessorHolder.class),
                mock(RegistrationFencingGuard.class), new DataSourceTransactionManager(dataSource));
        mockMvc = MockMvcBuilders.standaloneSetup(new DiscoveryController(service))
                .setControllerAdvice(new ExceptionHandlers(null))
                .build();
        insertSelector("selector-a", "ns-a");
        insertSelector("selector-b", "ns-a");
        insertSelector("selector-foreign", "ns-b");
        insertDiscovery("discovery-a", "0", "ns-a");
        insertDiscovery("discovery-b", "0", "ns-a");
        insertDiscovery("plugin-discovery", "1", "ns-a");
        insertDiscovery("foreign-discovery", "0", "ns-b");
        insertRelation("a", "discovery-a", "selector-a");
        insertRelation("b", "discovery-b", "selector-b");
        insertRelation("foreign", "foreign-discovery", "selector-foreign");
    }

    @AfterEach
    void tearDown() {
        if (Objects.nonNull(jdbc)) {
            jdbc.execute("SHUTDOWN");
        }
    }

    @Test
    void testSelectsEachDiscoveryWhenMultipleSelectorsHaveLocalConfig() throws Exception {
        insertRelation("duplicate", "discovery-a", "selector-a");
        query("divide", "0", "ns-a", "selector-a")
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.data.id").value("discovery-a"));
        query("divide", "0", "ns-a", "selector-b")
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.data.id").value("discovery-b"));
    }

    @Test
    void testReturnsNoDiscoveryFromAnotherNamespaceOrPlugin() throws Exception {
        insertRelation("mismatched-namespace", "discovery-a", "selector-foreign");
        jdbc.update("INSERT INTO discovery (id, discovery_name, discovery_level, plugin_name, namespace_id, discovery_type) "
                + "VALUES ('wrong-plugin', 'wrong-plugin', '0', 'grpc', 'ns-a', 'local')");
        insertRelation("mismatched-plugin", "wrong-plugin", "selector-a");
        query("divide", "0", "ns-a", "selector-foreign")
                .andExpect(jsonPath("$.code").value(200)).andExpect(jsonPath("$.data").value(nullValue()));
        query("divide", "0", "ns-a", "missing")
                .andExpect(jsonPath("$.code").value(200)).andExpect(jsonPath("$.data").value(nullValue()));
        query("grpc", "0", "ns-a", "selector-a")
                .andExpect(jsonPath("$.code").value(200)).andExpect(jsonPath("$.data").value(nullValue()));
    }

    @Test
    void testKeepsUnambiguousLegacyLookupsAndRejectsAmbiguousOnes() throws Exception {
        query("divide", "1", "ns-a", null).andExpect(jsonPath("$.data.id").value("plugin-discovery"));
        query("divide", "0", "ns-b", null).andExpect(jsonPath("$.data.id").value("foreign-discovery"));
        query("divide", "1", "ns-b", null).andExpect(jsonPath("$.data").value(nullValue()));
        query("divide", "0", "ns-a", null)
                .andExpect(jsonPath("$.code").value(400))
                .andExpect(jsonPath("$.message", containsString("selectorId")));
    }

    @Test
    void testRejectsBlankInapplicableAndAmbiguousSelectorScope() throws Exception {
        query("divide", "0", "ns-a", " ")
                .andExpect(jsonPath("$.code").value(400))
                .andExpect(jsonPath("$.message").value("selectorId must be nonblank."));
        query("divide", "1", "ns-a", "selector-a")
                .andExpect(jsonPath("$.code").value(400))
                .andExpect(jsonPath("$.message").value("selectorId is only supported for level=0."));
        insertRelation("ambiguous", "discovery-b", "selector-a");
        query("divide", "0", "ns-a", "selector-a").andExpect(jsonPath("$.code").value(400));
    }

    @Test
    void testDisambiguatesPluginDiscoveryByType() throws Exception {
        insertDiscovery("registry-discovery", "1", "ns-a", "zookeeper");
        insertDiscovery("foreign-registry", "1", "ns-b", "zookeeper");
        query("divide", "1", "ns-a", null)
                .andExpect(jsonPath("$.code").value(400))
                .andExpect(jsonPath("$.message", containsString("Specify discoveryType")));
        query("divide", "1", "ns-a", null, "zookeeper")
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.data.id").value("registry-discovery"));
        query("divide", "1", "ns-a", null, "local")
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.data.id").value("plugin-discovery"));
        query("divide", "1", "ns-a", null, "unconfigured")
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.data").value(nullValue()));
        insertDiscovery("duplicate-local", "1", "ns-a");
        query("divide", "1", "ns-a", null, "local")
                .andExpect(jsonPath("$.code").value(400))
                .andExpect(jsonPath("$.message", containsString("Resolve the duplicate definitions")));
    }

    @Test
    void testRejectsBlankAndInapplicableDiscoveryType() throws Exception {
        query("divide", "1", "ns-a", null, " ")
                .andExpect(jsonPath("$.code").value(400))
                .andExpect(jsonPath("$.message").value("discoveryType must be nonblank."));
        query("divide", "0", "ns-a", null, "local")
                .andExpect(jsonPath("$.code").value(400))
                .andExpect(jsonPath("$.message").value("discoveryType is only supported for level=1."));
    }

    @ParameterizedTest
    @ValueSource(strings = {"h2", "oracle"})
    void testDatabaseLimitsLookupToTwoMatches(final String databaseId) throws Exception {
        insertDiscovery("third-discovery", "0", "ns-a");
        try (SqlSession lookupSession = sessionFactory(jdbc.getDataSource(), databaseId).openSession()) {
            DiscoveryMapper mapper = lookupSession.getMapper(DiscoveryMapper.class);
            assertEquals(2, mapper.selectByPluginNameAndLevelAndNamespaceId("divide", "0", "ns-a", null, null).size());
        }
    }

    private SqlSessionFactory sessionFactory(final DataSource dataSource, final String databaseId) throws Exception {
        Configuration configuration = new Configuration(new Environment("test", new JdbcTransactionFactory(), dataSource));
        configuration.setDatabaseId(databaseId);
        try (InputStream mapper = new ClassPathResource("mappers/discovery-sqlmap.xml").getInputStream()) {
            new XMLMapperBuilder(mapper, configuration, "discovery", configuration.getSqlFragments()).parse();
        }
        return new SqlSessionFactoryBuilder().build(configuration);
    }

    private ResultActions query(final String plugin, final String level, final String namespace, final String selector) throws Exception {
        return query(plugin, level, namespace, selector, null);
    }

    private ResultActions query(final String plugin, final String level, final String namespace, final String selector, final String discoveryType) throws Exception {
        MockHttpServletRequestBuilder request = get("/discovery").param("pluginName", plugin).param("level", level).param("namespaceId", namespace);
        if (Objects.nonNull(selector)) {
            request.param("selectorId", selector);
        }
        if (Objects.nonNull(discoveryType)) {
            request.param("discoveryType", discoveryType);
        }
        return mockMvc.perform(request).andExpect(status().isOk());
    }

    private void insertSelector(final String id, final String namespace) {
        jdbc.update("INSERT INTO selector (id, plugin_id, selector_name, match_mode, selector_type, sort_code, enabled, logged, continued, match_restful, namespace_id) "
                + "VALUES (?, 'plugin', ?, 0, 0, 0, 1, 0, 0, 0, ?)", id, id, namespace);
    }

    private void insertDiscovery(final String id, final String level, final String namespace) {
        insertDiscovery(id, level, namespace, "local");
    }

    private void insertDiscovery(final String id, final String level, final String namespace, final String type) {
        jdbc.update("INSERT INTO discovery (id, discovery_name, discovery_level, plugin_name, namespace_id, discovery_type) "
                + "VALUES (?, ?, ?, 'divide', ?, ?)", id, id, level, namespace, type);
    }

    private void insertRelation(final String id, final String discovery, final String selector) {
        jdbc.update("INSERT INTO discovery_handler (id, discovery_id, handler) VALUES (?, ?, '{}')", id, discovery);
        jdbc.update("INSERT INTO discovery_rel (id, plugin_name, discovery_handler_id, selector_id) VALUES (?, 'divide', ?, ?)", id, id, selector);
    }
}
