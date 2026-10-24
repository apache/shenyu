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
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import org.apache.ibatis.transaction.jdbc.JdbcTransactionFactory;
import org.apache.shenyu.admin.model.entity.NamespaceDO;
import org.apache.shenyu.admin.model.query.NamespaceQuery;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;

import java.io.InputStream;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for namespace name filtering and namespace scope.
 */
public final class NamespaceMapperTest {

    @Test
    public void testNameFilteringAndNamespaceScope() throws Exception {
        DriverManagerDataSource dataSource = new DriverManagerDataSource(
                "jdbc:h2:mem:namespace-filter-" + UUID.randomUUID()
                        + ";DB_CLOSE_DELAY=-1;MODE=MySQL", "sa", "");
        JdbcTemplate jdbc = new JdbcTemplate(dataSource);

        try {
            new ResourceDatabasePopulator(
                    new ClassPathResource("sql-script/h2/schema.sql")).execute(dataSource);

            String insertSql = "INSERT INTO namespace (id, namespace_id, name) VALUES (?, ?, ?)";
            jdbc.update(insertSql, "test-foo", "ns-foo", "pr7703-foo");
            jdbc.update(insertSql, "test-prod-foo", "ns-prod-foo", "prod-pr7703-foo");
            jdbc.update(insertSql, "test-bar", "ns-bar", "pr7703-bar");

            Configuration configuration = new Configuration(
                    new Environment("test", new JdbcTransactionFactory(), dataSource));
            configuration.setMapUnderscoreToCamelCase(true);
            String mapperResource = "mappers/namespace-sqlmap.xml";
            try (InputStream input = new ClassPathResource(mapperResource).getInputStream()) {
                new XMLMapperBuilder(input, configuration, mapperResource,
                        configuration.getSqlFragments()).parse();
            }

            try (SqlSession session = new SqlSessionFactoryBuilder()
                    .build(configuration).openSession()) {
                NamespaceMapper mapper = session.getMapper(NamespaceMapper.class);
                NamespaceQuery query = new NamespaceQuery();
                query.setName("pr7703-foo");

                assertEquals(Set.of("ns-foo", "ns-prod-foo"),
                        namespaceIds(mapper.selectByQuery(query)));

                List<String> allowedNamespaces = List.of("ns-foo", "ns-bar");
                assertEquals(Set.of("ns-foo"), namespaceIds(
                        mapper.selectByNamespaceIdsAndName(allowedNamespaces, "pr7703-foo")));

                query.setName("pr7703-missing");
                assertTrue(mapper.selectByQuery(query).isEmpty());

                Set<String> allNamespaces = namespaceIds(mapper.selectAll());
                for (String name : new String[]{null, ""}) {
                    query.setName(name);
                    assertEquals(allNamespaces, namespaceIds(mapper.selectByQuery(query)));
                    assertEquals(Set.of("ns-foo", "ns-bar"), namespaceIds(
                            mapper.selectByNamespaceIdsAndName(allowedNamespaces, name)));
                }
            }
        } finally {
            jdbc.execute("SHUTDOWN");
        }
    }

    private Set<String> namespaceIds(final List<NamespaceDO> namespaces) {
        return namespaces.stream()
                .map(NamespaceDO::getNamespaceId)
                .collect(Collectors.toSet());
    }
}
