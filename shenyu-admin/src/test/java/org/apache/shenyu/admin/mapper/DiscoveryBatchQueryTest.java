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
import org.junit.jupiter.api.Test;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;

import java.io.InputStream;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DiscoveryBatchQueryTest {

    @Test
    void readsOnlyRowsForRequestedHandlers() throws Exception {
        DriverManagerDataSource dataSource = new DriverManagerDataSource(
                "jdbc:h2:mem:discovery-batch-" + UUID.randomUUID() + ";DB_CLOSE_DELAY=-1;MODE=MySQL", "sa", "");
        JdbcTemplate jdbc = new JdbcTemplate(dataSource);
        try {
            new ResourceDatabasePopulator(new ClassPathResource("sql-script/h2/schema.sql")).execute(dataSource);
            for (String id : List.of("a", "b", "c")) {
                jdbc.update("INSERT INTO discovery_rel (id, plugin_name, discovery_handler_id, selector_id) VALUES (?, 'divide', ?, ?)", id, id, "s-" + id);
                jdbc.update("INSERT INTO discovery_upstream (id, discovery_handler_id, namespace_id, upstream_url, upstream_status, weight) "
                        + "VALUES (?, ?, 'namespace', 'localhost:8080', 0, 50)", id, id);
            }
            Configuration configuration = new Configuration(new Environment("test", new JdbcTransactionFactory(), dataSource));
            for (String mapper : List.of("discovery-rel", "discovery-upstream")) {
                try (InputStream input = new ClassPathResource("mappers/" + mapper + "-sqlmap.xml").getInputStream()) {
                    new XMLMapperBuilder(input, configuration, mapper, configuration.getSqlFragments()).parse();
                }
            }
            try (SqlSession session = new SqlSessionFactoryBuilder().build(configuration).openSession()) {
                DiscoveryRelMapper relations = session.getMapper(DiscoveryRelMapper.class);
                DiscoveryUpstreamMapper upstreams = session.getMapper(DiscoveryUpstreamMapper.class);
                assertEquals(2, relations.selectByDiscoveryHandlerIds(List.of("a", "b")).size());
                assertEquals(2, upstreams.selectByDiscoveryHandlerIds(List.of("a", "b")).size());
                assertEquals("s-c", relations.selectByDiscoveryHandlerIds(List.of("c")).get(0).getSelectorId());
                assertEquals("c", upstreams.selectByDiscoveryHandlerIds(List.of("c")).get(0).getDiscoveryHandlerId());
                assertTrue(relations.selectByDiscoveryHandlerIds(List.of("missing")).isEmpty());
                assertTrue(upstreams.selectByDiscoveryHandlerIds(List.of("missing")).isEmpty());
            }
        } finally {
            jdbc.execute("SHUTDOWN");
        }
    }
}
