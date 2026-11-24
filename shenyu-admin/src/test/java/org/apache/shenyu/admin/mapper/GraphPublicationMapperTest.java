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

import org.apache.shenyu.admin.model.entity.GraphDraftDO;
import org.apache.shenyu.admin.model.entity.GraphRevisionDO;
import org.apache.shenyu.admin.service.impl.NamespaceUserServiceImpl;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mybatis.spring.SqlSessionFactoryBean;
import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.core.io.ClassPathResource;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GraphPublicationMapperTest {

    private GraphPublicationMapper mapper;

    private NamespaceUserServiceImpl members;

    private JdbcTemplate jdbc;

    private TransactionTemplate transactions;

    @BeforeEach
    void setUp() throws Exception {
        DriverManagerDataSource dataSource = new DriverManagerDataSource(
                "jdbc:h2:mem:graph-" + UUID.randomUUID() + ";MODE=MySQL;DB_CLOSE_DELAY=-1", "sa", "");
        new ResourceDatabasePopulator(new ClassPathResource("sql-script/h2/schema.sql")).execute(dataSource);
        jdbc = new JdbcTemplate(dataSource);
        transactions = new TransactionTemplate(new DataSourceTransactionManager(dataSource));
        SqlSessionFactoryBean factory = new SqlSessionFactoryBean();
        factory.setDataSource(dataSource);
        factory.setMapperLocations(new ClassPathResource("mappers/graph-publication-sqlmap.xml"), new ClassPathResource("mappers/namespace-user-rel-sqlmap.xml"));
        SqlSessionTemplate sessions = new SqlSessionTemplate(factory.getObject());
        mapper = sessions.getMapper(GraphPublicationMapper.class);
        members = new NamespaceUserServiceImpl(sessions.getMapper(NamespaceUserRelMapper.class));
        seedNamespace("one", "ns");
    }

    @AfterEach
    void tearDown() {
        jdbc.execute("SHUTDOWN");
    }

    @Test
    void onlyOneOfTwoConcurrentEditsSucceeds() throws Exception {
        mapper.insertDraft(draft("ns", 1, 0, "{}"));
        final CyclicBarrier start = new CyclicBarrier(2);
        Callable<Integer> update = () -> {
            GraphDraftDO draft = mapper.findDraft("ns", "graph");
            assertEquals(1L, draft.getDraftVersion());
            start.await(5, TimeUnit.SECONDS);
            draft.setDraftVersion(2);
            return mapper.updateDraft(draft, 1, 0);
        };
        ExecutorService workers = Executors.newFixedThreadPool(2);
        try {
            Future<Integer> first = workers.submit(update);
            Future<Integer> second = workers.submit(update);
            assertEquals(1, first.get(10, TimeUnit.SECONDS) + second.get(10, TimeUnit.SECONDS));
            assertEquals(2L, mapper.findDraft("ns", "graph").getDraftVersion());
        } finally {
            workers.shutdownNow();
        }
    }

    @Test
    void historyAndLatestTombstoneRemainScopedToTheNamespaceLifetime() {
        seedNamespace("two", "NS");
        assertEquals(0, mapper.insertDraft(draft("missing", 1, 0, "{}")));
        String largeJson = "\"" + "x".repeat(8000) + "\"";
        mapper.insertDraft(draft("ns", 1, 0, largeJson));
        mapper.insertDraft(draft("NS", 1, 0, "{}"));
        assertEquals(largeJson, mapper.findDraft("ns", "graph").getDraftJson());
        assertTrue(mapper.listPublished("ns").isEmpty());
        assertEquals(0, mapper.updateDraft(draft("ns", 2, 1, "{}"), 1, 4));

        transactions.executeWithoutResult(status -> {
            assertEquals(1, mapper.updateDraft(draft("ns", 2, 1, largeJson), 1, 0));
            mapper.insertRevision(revision(1, largeJson));
        });
        assertEquals(largeJson, mapper.listPublished("ns").get(0).getGraphJson());
        assertThrows(DuplicateKeyException.class, () -> mapper.insertRevision(revision(1, "replacement")));
        transactions.executeWithoutResult(status -> {
            assertEquals(1, mapper.updateDraft(draft("ns", 3, 2, null), 2, 1));
            mapper.insertRevision(revision(2, null));
        });
        assertEquals(largeJson, mapper.findRevision("ns", "graph", 1).getGraphJson());
        assertEquals(List.of(1L, 2L), mapper.listRevisions("ns", "graph").stream().map(GraphRevisionDO::getRevision).toList());
        List<GraphRevisionDO> latest = mapper.listPublished("ns");
        assertEquals(1, latest.size());
        assertEquals(2L, latest.get(0).getRevision());
        assertNull(latest.get(0).getGraphJson());
        assertNull(mapper.findRevision("NS", "graph", 1));
        assertTrue(mapper.listPublished("NS").isEmpty());

        jdbc.update("DELETE FROM namespace WHERE id = ?", "one");
        seedNamespace("three", "ns");
        assertNull(mapper.findDraft("ns", "graph"));
        assertTrue(mapper.listRevisions("ns", "graph").isEmpty());
        assertEquals(1L, mapper.findDraft("NS", "graph").getDraftVersion());
    }

    @Test
    void recreatedNamespaceRequiresANewMembershipGrant() {
        members.create("ns", "old-editor");
        assertThrows(DuplicateKeyException.class, () -> seedNamespace("duplicate", "ns"));
        transactions.executeWithoutResult(status -> assertTrue(members.lockMembership("ns", "old-editor")));

        jdbc.update("DELETE FROM namespace WHERE id = ?", "one");
        seedNamespace("new", "ns");
        members.create("ns", "new-editor");
        transactions.executeWithoutResult(status -> {
            assertFalse(members.lockMembership("ns", "old-editor"));
            assertTrue(members.lockMembership("ns", "new-editor"));
        });
    }

    @Test
    void failureDuringPublicationRollsBackBothWrites() {
        mapper.insertDraft(draft("ns", 1, 0, "{}"));
        assertThrows(DuplicateKeyException.class, () -> transactions.executeWithoutResult(status -> {
            mapper.updateDraft(draft("ns", 2, 1, "{}"), 1, 0);
            mapper.insertRevision(revision(1, "{}"));
            mapper.insertRevision(revision(1, "duplicate"));
        }));
        GraphDraftDO head = mapper.findDraft("ns", "graph");
        assertEquals(1L, head.getDraftVersion());
        assertEquals(0L, head.getPublishedRevision());
        assertTrue(mapper.listRevisions("ns", "graph").isEmpty());
    }

    private void seedNamespace(final String id, final String namespaceId) {
        jdbc.update("INSERT INTO namespace (id, namespace_id, name) VALUES (?, ?, ?)", id, namespaceId, namespaceId);
    }

    private GraphDraftDO draft(final String namespaceId, final long version, final long published, final String json) {
        GraphDraftDO draft = new GraphDraftDO();
        draft.setNamespaceId(namespaceId);
        draft.setGraphId("graph");
        draft.setDraftVersion(version);
        draft.setPublishedRevision(published);
        draft.setDraftJson(json);
        return draft;
    }

    private GraphRevisionDO revision(final long number, final String json) {
        GraphRevisionDO revision = new GraphRevisionDO();
        revision.setNamespaceId("ns");
        revision.setGraphId("graph");
        revision.setRevision(number);
        revision.setGraphJson(json);
        return revision;
    }
}
