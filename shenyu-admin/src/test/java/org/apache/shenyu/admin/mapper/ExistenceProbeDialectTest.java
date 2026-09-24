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
import org.apache.ibatis.mapping.Environment;
import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import org.apache.ibatis.transaction.jdbc.JdbcTransactionFactory;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;

import java.io.InputStream;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verify existence probes retain their nullable Boolean contract across dialects.
 */
public class ExistenceProbeDialectTest {

    @ParameterizedTest
    @ValueSource(strings = {"oracle", "h2"})
    public void testExistenceProbes(final String databaseId) throws Exception {
        DriverManagerDataSource dataSource = new DriverManagerDataSource(
                "jdbc:h2:mem:probes-" + UUID.randomUUID() + ";DB_CLOSE_DELAY=-1;MODE=MySQL", "sa", "");
        JdbcTemplate jdbc = new JdbcTemplate(dataSource);
        try {
            new ResourceDatabasePopulator(new ClassPathResource("sql-script/h2/schema.sql")).execute(dataSource);
            if ("oracle".equals(databaseId)) {
                jdbc.execute("SET MODE Oracle");
            }
            Configuration configuration = new Configuration(new Environment("test", new JdbcTransactionFactory(), dataSource));
            configuration.setDatabaseId(databaseId);
            for (Resource resource : new PathMatchingResourcePatternResolver().getResources("classpath*:mappers/*-sqlmap.xml")) {
                try (InputStream input = resource.getInputStream()) {
                    new XMLMapperBuilder(input, configuration, resource.toString(), configuration.getSqlFragments()).parse();
                }
            }
            Map<String, Object> parameters = Map.of("id", "missing-probe", "name", "missing-probe", "path", "missing-probe",
                    "userId", "missing-probe", "authId", "missing-probe", "appKey", "missing-probe",
                    "namespaceId", "missing-probe", "exclude", List.of("excluded-probe"));
            List<MappedStatement> probes = configuration.getMappedStatementNames().stream()
                    .filter(name -> name.contains("."))
                    .map(configuration::getMappedStatement)
                    .filter(statement -> !statement.getResultMaps().isEmpty() && statement.getResultMaps().get(0).getType() == Boolean.class)
                    .filter(statement -> statement.getBoundSql(parameters).getSql().stripLeading().toUpperCase(Locale.ROOT).startsWith("SELECT 1"))
                    .collect(Collectors.toList());
            assertEquals(39, probes.size());
            try (SqlSession session = new SqlSessionFactoryBuilder().build(configuration).openSession()) {
                for (MappedStatement probe : probes) {
                    if ("oracle".equals(databaseId)) {
                        assertEquals("oracle", probe.getDatabaseId(), probe.getId());
                        assertFalse(probe.getBoundSql(parameters).getSql().toUpperCase(Locale.ROOT).contains("LIMIT"), probe.getId());
                    }
                    assertNull(session.selectOne(probe.getId(), parameters), probe.getId());
                }
                jdbc.update("INSERT INTO plugin (id, name, role) VALUES ('probe-1', 'probe-name', 'test')");
                jdbc.update("INSERT INTO plugin (id, name, role) VALUES ('probe-2', 'probe-name', 'test')");
                assertTrue(session.getMapper(PluginMapper.class).existed("probe-1"));
                assertTrue(session.getMapper(PluginMapper.class).nameExisted("probe-name"));
                assertNull(session.getMapper(PluginMapper.class).nameExisted("missing-probe"));
            }
        } finally {
            jdbc.execute("SHUTDOWN");
        }
    }
}
