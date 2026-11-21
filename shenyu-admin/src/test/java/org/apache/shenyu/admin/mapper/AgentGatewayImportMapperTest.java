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

import org.apache.ibatis.builder.xml.XMLMapperBuilder;
import org.apache.ibatis.exceptions.PersistenceException;
import org.apache.ibatis.mapping.Environment;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import org.apache.ibatis.transaction.jdbc.JdbcTransactionFactory;
import org.apache.shenyu.admin.model.entity.AgentGatewayImportDO;
import org.apache.shenyu.admin.model.entity.OperationRecordLog;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;

import java.io.InputStream;
import java.sql.Timestamp;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

/** Durable provenance, namespace filtering and locking on the real H2/MyBatis path. */
class AgentGatewayImportMapperTest {

    @Test
    void manifestUpdatePreservesIdentityAndRollsBackWithRuleChanges() throws Exception {
        SqlSessionFactory factory = database();
        String manifest = "{\"source\":\"" + "x".repeat(100000) + "\"}";
        Timestamp created = Timestamp.valueOf("2026-10-31 00:00:00");
        Timestamp updated = Timestamp.valueOf("2026-10-31 00:00:01");
        try (SqlSession session = factory.openSession(false)) {
            AgentGatewayImportMapper imports = session.getMapper(AgentGatewayImportMapper.class);
            RuleMapper rules = session.getMapper(RuleMapper.class);
            assertEquals("agent", rules.selectByIdForUpdate("agent").getId());
            assertEquals(1, imports.insertSelective(importRecord("import-1", manifest, created)));
            assertEquals(manifest, imports.selectByRuleId("agent", "ns").getManifest());
            assertNull(imports.selectByRuleId("agent", "other"));
            assertEquals(1, imports.updateManifestById("import-1", "{\"v\":2}", updated));
            AgentGatewayImportDO saved = imports.selectByRuleId("agent", "ns");
            assertEquals("import-1", saved.getId());
            assertEquals(created, saved.getDateCreated());
            assertEquals(updated, saved.getDateUpdated());
            assertEquals("{\"v\":2}", saved.getManifest());
            RuleDO update = new RuleDO();
            update.setId("agent");
            update.setNamespaceId("ns");
            update.setHandle(manifest);
            assertEquals(1, rules.updateSelective(update));
            assertEquals(manifest, rules.selectById("agent").getHandle());
            session.rollback();
        }
        try (SqlSession session = factory.openSession(false)) {
            assertNull(session.getMapper(AgentGatewayImportMapper.class).selectByRuleId("agent", "ns"));
            assertEquals("{}", session.getMapper(RuleMapper.class).selectById("agent").getHandle());
            session.getMapper(AgentGatewayImportMapper.class).insertSelective(importRecord("import-1", manifest, created));
            session.commit();
        }
        try (SqlSession session = factory.openSession(false)) {
            assertEquals(manifest, session.getMapper(AgentGatewayImportMapper.class).selectByRuleId("agent", "ns").getManifest());
            assertEquals(1, session.getMapper(AgentGatewayImportMapper.class).deleteByRuleId("agent", "ns"));
            session.commit();
        }
    }

    private AgentGatewayImportDO importRecord(final String id, final String manifest, final Timestamp time) {
        AgentGatewayImportDO record = new AgentGatewayImportDO();
        record.setId(id);
        record.setRuleId("agent");
        record.setNamespaceId("ns");
        record.setManifest(manifest);
        record.setDateCreated(time);
        record.setDateUpdated(time);
        return record;
    }

    @Test
    void parentRuleLockSerializesOrdinaryEditors() throws Exception {
        SqlSessionFactory factory = database();
        try (SqlSession importing = factory.openSession(false); SqlSession editing = factory.openSession(false)) {
            importing.getMapper(RuleMapper.class).selectByIdForUpdate("agent");
            try (java.sql.Statement statement = editing.getConnection().createStatement()) {
                statement.execute("SET LOCK_TIMEOUT 100");
            }
            RuleDO update = new RuleDO();
            update.setId("agent");
            update.setNamespaceId("ns");
            update.setHandle("{\"manual\":true}");
            assertThrows(PersistenceException.class, () -> editing.getMapper(RuleMapper.class).updateSelective(update));
            // A read-only session is not "dirty", so plain rollback() is a no-op and keeps the row lock.
            importing.rollback(true);
            editing.rollback();
            assertEquals(1, editing.getMapper(RuleMapper.class).updateSelective(update));
        }
    }

    @Test
    void largeOperationLogContextRoundTripsThroughTheMapper() throws Exception {
        SqlSessionFactory factory = database();
        String context = "x".repeat(100000);
        try (SqlSession session = factory.openSession(false)) {
            OperationRecordLog log = new OperationRecordLog();
            log.setColor("red");
            log.setContext(context);
            log.setOperator("large-context");
            log.setOperationTime(new Date());
            log.setOperationType("update");
            assertEquals(1, session.getMapper(OperationRecordLogMapper.class).insert(log));
            session.commit();
        }
        try (SqlSession session = factory.openSession(false)) {
            List<OperationRecordLog> logs = session.getMapper(OperationRecordLogMapper.class).selectLimit("large-context", 1);
            assertEquals(1, logs.size());
            assertEquals(context, logs.get(0).getContext());
            session.rollback();
        }
    }

    private SqlSessionFactory database() throws Exception {
        DriverManagerDataSource dataSource = new DriverManagerDataSource("jdbc:h2:mem:agent-import-" + UUID.randomUUID() + ";DB_CLOSE_DELAY=-1;MODE=MySQL", "sa", "");
        new ResourceDatabasePopulator(new ClassPathResource("sql-script/h2/schema.sql")).execute(dataSource);
        new JdbcTemplate(dataSource).update("INSERT INTO rule (id, selector_id, match_mode, rule_name, enabled, logged, match_restful, namespace_id, sort_code, handle) "
                + "VALUES ('agent', 'selector', 0, 'agent', 1, 0, 0, 'ns', 0, '{}')");
        Configuration configuration = new Configuration(new Environment("test", new JdbcTransactionFactory(), dataSource));
        configuration.addMapper(AgentGatewayImportMapper.class);
        for (String resource : new String[] {"mappers/rule-sqlmap.xml", "mappers/agent-gateway-import-sqlmap.xml",
            "mappers/operation-record-log-sqlmap.xml"}) {
            try (InputStream input = new ClassPathResource(resource).getInputStream()) {
                new XMLMapperBuilder(input, configuration, resource, configuration.getSqlFragments()).parse();
            }
        }
        return new SqlSessionFactoryBuilder().build(configuration);
    }
}
