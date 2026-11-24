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

import org.apache.shenyu.admin.mapper.GraphPublicationMapper;
import org.apache.shenyu.admin.model.entity.GraphRevisionDO;
import org.apache.shenyu.common.dto.GraphData;
import org.apache.shenyu.common.dto.GraphNodeData;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mybatis.spring.SqlSessionFactoryBean;
import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.aop.framework.ProxyFactory;
import org.springframework.core.io.ClassPathResource;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;
import org.springframework.transaction.annotation.AnnotationTransactionAttributeSource;
import org.springframework.transaction.interceptor.TransactionInterceptor;
import org.springframework.web.server.ResponseStatusException;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class GraphPublicationTransactionTest {

    private static final String GRAPH = "checkout";

    private DriverManagerDataSource dataSource;

    private JdbcTemplate jdbc;

    @BeforeEach
    void setUp() {
        dataSource = new DriverManagerDataSource("jdbc:h2:mem:graph-" + UUID.randomUUID() + ";MODE=MySQL;DB_CLOSE_DELAY=-1", "sa", "");
        new ResourceDatabasePopulator(new ClassPathResource("sql-script/h2/schema.sql")).execute(dataSource);
        jdbc = new JdbcTemplate(dataSource);
    }

    @AfterEach
    void tearDown() {
        jdbc.execute("SHUTDOWN");
    }

    @Test
    void failedRevisionInsertRollsBackHeadAndASecondAdminRecoversCommittedState() throws Exception {
        GraphPublicationService writer = service();
        writer.createDraft(SYS_DEFAULT_NAMESPACE_ID, GRAPH, definition());
        jdbc.execute("ALTER TABLE plugin_graph_revision ADD CONSTRAINT fail_insert CHECK (revision > 1)");
        assertThrows(DataIntegrityViolationException.class, () -> writer.publish(SYS_DEFAULT_NAMESPACE_ID, GRAPH, 1));

        GraphPublicationService reader = service();
        assertEquals(1L, reader.getDraft(SYS_DEFAULT_NAMESPACE_ID, GRAPH).getDraftVersion());
        assertEquals(0L, reader.getDraft(SYS_DEFAULT_NAMESPACE_ID, GRAPH).getPublishedRevision());
        assertEquals(List.of(), reader.snapshot(SYS_DEFAULT_NAMESPACE_ID));
        jdbc.execute("ALTER TABLE plugin_graph_revision DROP CONSTRAINT fail_insert");

        GraphRevisionDO published = writer.publish(SYS_DEFAULT_NAMESPACE_ID, GRAPH, 1);
        assertEquals(1L, published.getRevision());
        assertEquals(published.getGraphJson(), reader.snapshot(SYS_DEFAULT_NAMESPACE_ID).get(0).getGraphJson());
        writer.delete(SYS_DEFAULT_NAMESPACE_ID, GRAPH, 2);
        GraphRevisionDO withdrawal = reader.snapshot(SYS_DEFAULT_NAMESPACE_ID).get(0);
        assertEquals(2L, withdrawal.getRevision());
        assertNull(withdrawal.getGraphJson());
        assertNotNull(reader.rollback(SYS_DEFAULT_NAMESPACE_ID, GRAPH, 3, 1).getGraphJson());
        assertEquals(3L, writer.snapshot(SYS_DEFAULT_NAMESPACE_ID).get(0).getRevision());
        assertEquals(published.getGraphJson(), reader.getRevision(SYS_DEFAULT_NAMESPACE_ID, GRAPH, 1).getGraphJson());
    }

    @Test
    void onlyOneAdminCanPublishTheSameDraftVersion() throws Exception {
        GraphPublicationService first = service();
        GraphPublicationService second = service();
        first.createDraft(SYS_DEFAULT_NAMESPACE_ID, GRAPH, definition());
        CyclicBarrier start = new CyclicBarrier(2);
        ExecutorService workers = Executors.newFixedThreadPool(2);
        try {
            Future<Integer> one = workers.submit(publish(first, start));
            Future<Integer> two = workers.submit(publish(second, start));
            List<Integer> statuses = List.of(one.get(10, TimeUnit.SECONDS), two.get(10, TimeUnit.SECONDS));
            assertEquals(List.of(200, 409), statuses.stream().sorted().toList());
            assertEquals(1, second.history(SYS_DEFAULT_NAMESPACE_ID, GRAPH).size());
        } finally {
            workers.shutdownNow();
        }
    }

    private Callable<Integer> publish(final GraphPublicationService service, final CyclicBarrier start) {
        return () -> {
            start.await(5, TimeUnit.SECONDS);
            try {
                service.publish(SYS_DEFAULT_NAMESPACE_ID, GRAPH, 1);
                return 200;
            } catch (ResponseStatusException ex) {
                return ex.getStatusCode().value();
            }
        };
    }

    private GraphData definition() {
        GraphNodeData end = new GraphNodeData();
        end.setNodeId("end");
        end.setType("END");
        GraphData graph = new GraphData();
        graph.setSchemaVersion(1);
        graph.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        graph.setGraphId(GRAPH);
        graph.setDeploymentBinding("canary");
        graph.setEntryNodeId(end.getNodeId());
        graph.setNodes(List.of(end));
        return graph;
    }

    private GraphPublicationService service() throws Exception {
        SqlSessionFactoryBean factory = new SqlSessionFactoryBean();
        factory.setDataSource(dataSource);
        factory.setMapperLocations(new ClassPathResource("mappers/graph-publication-sqlmap.xml"));
        SqlSessionTemplate sessions = new SqlSessionTemplate(factory.getObject());
        ProxyFactory proxy = new ProxyFactory(new GraphPublicationService(sessions.getMapper(GraphPublicationMapper.class)));
        proxy.addAdvice(new TransactionInterceptor(new DataSourceTransactionManager(dataSource), new AnnotationTransactionAttributeSource()));
        return (GraphPublicationService) proxy.getProxy();
    }
}
