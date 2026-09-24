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
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;

import java.io.InputStream;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DiscoveryNamespaceQueryTest {

    @Test
    void scopesRegularAndProxyHandlersAtTheDatabase() throws Exception {
        DriverManagerDataSource dataSource = new DriverManagerDataSource(
                "jdbc:h2:mem:discovery-namespace-" + UUID.randomUUID() + ";DB_CLOSE_DELAY=-1;MODE=MySQL", "sa", "");
        JdbcTemplate jdbc = new JdbcTemplate(dataSource);
        try {
            new ResourceDatabasePopulator(new ClassPathResource("sql-script/h2/schema.sql")).execute(dataSource);
            for (String namespace : new String[]{"a", "b"}) {
                jdbc.update("INSERT INTO selector (id, plugin_id, selector_name, match_mode, selector_type, sort_code, enabled, loged, continued, match_restful, namespace_id) "
                        + "VALUES (?, 'p', 'selector', 0, 0, 0, 1, 0, 0, 0, ?)", "s-" + namespace, namespace);
                jdbc.update("INSERT INTO discovery_handler (id, discovery_id, handler) VALUES (?, 'discovery', '{}')", "h-" + namespace);
                jdbc.update("INSERT INTO discovery_rel (id, plugin_name, discovery_handler_id, selector_id) VALUES (?, 'divide', ?, ?)",
                        "r-" + namespace, "h-" + namespace, "s-" + namespace);
            }
            jdbc.update("INSERT INTO proxy_selector (id, name, plugin_name, type, forward_port, namespace_id) VALUES ('proxy-a', 'proxy', 'tcp', 'tcp', 10000, 'a')");
            jdbc.update("INSERT INTO discovery_handler (id, discovery_id, handler) VALUES ('h-proxy', 'discovery', '{}')");
            jdbc.update("INSERT INTO discovery_rel (id, plugin_name, discovery_handler_id, proxy_selector_id) VALUES ('r-proxy', 'tcp', 'h-proxy', 'proxy-a')");
            jdbc.update("INSERT INTO discovery_handler (id, discovery_id, handler) VALUES ('orphan', 'discovery', '{}')");
            Configuration configuration = new Configuration(new Environment("test", new JdbcTransactionFactory(), dataSource));
            try (InputStream input = new ClassPathResource("mappers/discovery-handler-sqlmap.xml").getInputStream()) {
                new XMLMapperBuilder(input, configuration, "discovery-handler", configuration.getSqlFragments()).parse();
            }
            try (SqlSession session = new SqlSessionFactoryBuilder().build(configuration).openSession()) {
                DiscoveryHandlerMapper mapper = session.getMapper(DiscoveryHandlerMapper.class);
                assertEquals(Set.of("h-a", "h-proxy"), mapper.selectAllByNamespaceId("a").stream().map(DiscoveryHandlerDO::getId).collect(Collectors.toSet()));
                assertEquals(Set.of("h-b"), mapper.selectAllByNamespaceId("b").stream().map(DiscoveryHandlerDO::getId).collect(Collectors.toSet()));
                assertTrue(mapper.selectAllByNamespaceId("missing").isEmpty());
            }
        } finally {
            jdbc.execute("SHUTDOWN");
        }
    }
}
