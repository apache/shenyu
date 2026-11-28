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

package org.apache.shenyu.admin.service.support;

import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.service.DataPermissionChecker;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.fencing.RegistrationFencingGuard;
import org.apache.shenyu.admin.service.impl.SelectorServiceImpl;
import org.apache.shenyu.admin.service.permission.PluginDataPermissionChecker;
import org.apache.shenyu.admin.service.publish.SelectorEventPublisher;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.ArgumentCaptor;
import org.mybatis.spring.SqlSessionFactoryBean;
import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.aop.framework.ProxyFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;
import org.springframework.transaction.annotation.AnnotationTransactionAttributeSource;
import org.springframework.transaction.interceptor.TransactionInterceptor;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.AdditionalAnswers.delegatesTo;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

class AiPassthroughSelectorGuardTest {

    private static final String NAMESPACE = "passthrough-test";

    private static final String NATIVE = "{\"mode\":\"passthrough\",\"baseUrl\":\"https://vendor.example\"}";

    private JdbcTemplate jdbc;

    private SelectorService service;

    private SelectorConditionMapper conditionMapper;

    @BeforeEach
    void setUp() throws Exception {
        DriverManagerDataSource source = new DriverManagerDataSource("jdbc:h2:mem:passthrough-" + UUID.randomUUID()
                + ";MODE=MySQL;DB_CLOSE_DELAY=-1;LOCK_TIMEOUT=5000", "sa", "");
        new ResourceDatabasePopulator(new ClassPathResource("sql-script/h2/schema.sql")).execute(source);
        jdbc = new JdbcTemplate(source);
        SqlSessionFactoryBean factory = new SqlSessionFactoryBean();
        factory.setDataSource(source);
        factory.setMapperLocations(new ClassPathResource("mappers/plugin-sqlmap.xml"), new ClassPathResource("mappers/selector-sqlmap.xml"),
                new ClassPathResource("mappers/selector-condition-sqlmap.xml"));
        SqlSessionTemplate sessions = new SqlSessionTemplate(factory.getObject());
        conditionMapper = mock(SelectorConditionMapper.class, delegatesTo(sessions.getMapper(SelectorConditionMapper.class)));
        SelectorServiceImpl target = new SelectorServiceImpl(sessions.getMapper(SelectorMapper.class), conditionMapper,
                sessions.getMapper(PluginMapper.class), mock(ApplicationEventPublisher.class), mock(DiscoveryMapper.class), mock(DiscoveryHandlerMapper.class),
                mock(DiscoveryRelMapper.class), mock(DiscoveryUpstreamMapper.class), mock(DiscoveryProcessorHolder.class), mock(SelectorEventPublisher.class),
                mock(DataPermissionChecker.class), mock(RegistrationFencingGuard.class), mock(PluginDataPermissionChecker.class));
        ProxyFactory proxy = new ProxyFactory(target);
        proxy.addAdvice(new TransactionInterceptor(new DataSourceTransactionManager(source), new AnnotationTransactionAttributeSource()));
        service = (SelectorService) proxy.getProxy();
    }

    @AfterEach
    void tearDown() {
        jdbc.execute("SHUTDOWN");
    }

    @Test
    void excludesOverlapsInEitherOrderAndKeepsIsolatedRoutes() {
        SelectorDTO prompt = selector("aiPrompt", "/chat/**");
        SelectorDTO nativeRoute = selector("aiProxy", "/claude/**");
        assertEquals(1, service.create(prompt));
        assertEquals(1, service.create(nativeRoute));

        prompt.setSelectorConditions(conditions("/claude/**"));
        assertConflict(() -> service.update(prompt), prompt, nativeRoute);
        SelectorDTO pii = selector("piiDetection", "/claude/**");
        assertConflict(() -> service.importData(List.of(pii)), pii, nativeRoute);
        nativeRoute.setSelectorConditions(conditions("/chat/**"));
        assertConflict(() -> service.update(nativeRoute), prompt, nativeRoute);

        SelectorDTO rewrite = selector("rewrite", "/whatever/**");
        assertConflict(() -> service.create(rewrite), prompt, nativeRoute);
        SelectorDTO transformer = selector("aiRequestTransformer", "/elsewhere/**");
        assertThrows(ShenyuAdminException.class, () -> service.create(transformer));
        assertEquals(2, count());
    }

    @ParameterizedTest
    @CsvSource({
        "match, /claude/a/**, /claude/b/**, true",
        "match, /claude/v1/a/**, /claude/v1/ab/**, true",
        "=, /claude/a, /claude/b/**, true",
        "startsWith, /claude/a/, /claude/ab/**, true",
        "startsWith, /claude/a, /claude/ab/**, false",
        "match, /claude/**, /claude/b/**, false",
        "match, /claude/b/**, /claude/**, false",
        "match, /claude/b*, /claude/beta/**, false",
        "match, /claude/*/x, /claude/b/**, false",
        "match, /claude/{vendor}/x, /claude/b/**, false"
    })
    void savesOnlySelectorsWithDisjointPathSegments(final String operator, final String promptPath, final String nativePath, final boolean disjoint) {
        SelectorDTO nativeRoute = selector("aiProxy", nativePath);
        SelectorDTO prompt = selector("aiPrompt", promptPath);
        prompt.getSelectorConditions().get(0).setOperator(operator);
        assertEquals(1, service.create(nativeRoute));
        if (disjoint) {
            assertEquals(1, service.create(prompt));
        } else {
            assertConflict(() -> service.create(prompt), prompt, nativeRoute);
        }
    }

    @Test
    void rejectsActivationAndDynamicMatchesAndValidatesTheModeOnImport() {
        SelectorDTO nativeRoute = selector("aiProxy", "/claude/**");
        assertEquals(1, service.create(nativeRoute));
        SelectorDTO prompt = selector("aiPrompt", "/claude/**");
        prompt.setEnabled(false);
        assertEquals(1, service.create(prompt));
        assertConflict(() -> service.enabledByIdsAndNamespaceId(List.of(prompt.getId()), true, NAMESPACE), prompt, nativeRoute);
        assertEquals(0, jdbc.queryForObject("SELECT enabled FROM selector WHERE id=?", Integer.class, prompt.getId()));
        SelectorDTO dynamic = selector("aiPrompt", "/other/**");
        dynamic.getSelectorConditions().get(0).setParamType("header");
        assertConflict(() -> service.create(dynamic), dynamic, nativeRoute);
        SelectorDTO invalid = selector("aiProxy", "/other/**");
        invalid.setHandle("{\"mode\":\"unknown\"}");
        assertThrows(ShenyuAdminException.class, () -> service.importData(List.of(invalid)));
        jdbc.update("UPDATE selector SET enabled = 0, handle = ? WHERE id = ?", invalid.getHandle(), nativeRoute.getId());
        assertThrows(ShenyuAdminException.class, () -> service.enabledByIdsAndNamespaceId(List.of(nativeRoute.getId()), true, NAMESPACE));
        assertEquals(2, count());
    }

    @Test
    @SuppressWarnings("unchecked")
    void moreThanOneThousandPoliciesAreCheckedInBoundedBatches() {
        SelectorDTO route = selector("aiProxy", "/claude/**");
        String promptId = jdbc.queryForObject("SELECT id FROM plugin WHERE name='aiPrompt'", String.class);
        jdbc.update("INSERT INTO selector (id, plugin_id, selector_name, match_mode, selector_type, sort_code, enabled, logged, continued, match_restful, namespace_id)"
                + " SELECT CONCAT('body-', x), ?, CONCAT('body-', x), 0, 1, 0, 1, 0, 0, 0, ? FROM SYSTEM_RANGE(1, 1001)", promptId, NAMESPACE);
        jdbc.update("INSERT INTO selector_condition (id, selector_id, param_type, operator, param_name, param_value)"
                + " SELECT CONCAT('condition-', x), CONCAT('body-', x), 'uri', 'match', '/', '/chat/**' FROM SYSTEM_RANGE(1, 1001)");
        assertEquals(1, service.create(route));
        ArgumentCaptor<List<String>> batches = ArgumentCaptor.forClass(List.class);
        verify(conditionMapper, times(3)).selectBySelectorIdsForUpdate(batches.capture());
        assertEquals(List.of(500, 500, 1), batches.getAllValues().stream().map(List::size).toList());
        assertEquals(1001L, batches.getAllValues().stream().flatMap(List::stream).distinct().count());
    }

    @Test
    void simultaneousOverlappingCreatesOnlyCommitOneSelector() throws Exception {
        ExecutorService workers = Executors.newFixedThreadPool(2);
        try {
            CyclicBarrier start = new CyclicBarrier(2);
            SelectorDTO route = selector("aiProxy", "/claude/**");
            SelectorDTO prompt = selector("aiPrompt", "/claude/**");
            Future<Boolean> first = workers.submit(() -> saveAtBarrier(start, route));
            Future<Boolean> second = workers.submit(() -> saveAtBarrier(start, prompt));
            assertNotEquals(first.get(10, TimeUnit.SECONDS), second.get(10, TimeUnit.SECONDS));
            assertEquals(1, count());
        } finally {
            workers.shutdownNow();
        }
    }

    private boolean saveAtBarrier(final CyclicBarrier barrier, final SelectorDTO selector) throws Exception {
        barrier.await(5, TimeUnit.SECONDS);
        try {
            service.create(selector);
            return true;
        } catch (ShenyuAdminException e) {
            assertTrue(e.getMessage().contains("may overlap"));
            return false;
        }
    }

    private SelectorDTO selector(final String plugin, final String path) {
        String id = UUID.randomUUID().toString();
        SelectorDTO dto = new SelectorDTO();
        dto.setId(id);
        dto.setPluginId(jdbc.queryForObject("SELECT id FROM plugin WHERE name=?", String.class, plugin));
        dto.setName(id);
        dto.setNamespaceId(NAMESPACE);
        dto.setEnabled(true);
        dto.setLogged(false);
        dto.setContinued(false);
        dto.setMatchRestful(false);
        dto.setSort(0);
        dto.setType(1);
        dto.setMatchMode(0);
        dto.setHandle("aiProxy".equals(plugin) ? NATIVE : "{}");
        dto.setSelectorConditions(conditions(path));
        return dto;
    }

    private List<SelectorConditionDTO> conditions(final String path) {
        SelectorConditionDTO condition = new SelectorConditionDTO();
        condition.setParamType("uri");
        condition.setOperator("match");
        condition.setParamName("/");
        condition.setParamValue(path);
        return List.of(condition);
    }

    private void assertConflict(final Runnable save, final SelectorDTO policy, final SelectorDTO route) {
        ShenyuAdminException error = assertThrows(ShenyuAdminException.class, save::run);
        assertTrue(error.getMessage().contains(policy.getId()));
        assertTrue(error.getMessage().contains(route.getId()));
    }

    private int count() {
        return jdbc.queryForObject("SELECT count(*) FROM selector WHERE namespace_id=?", Integer.class, NAMESPACE);
    }
}
