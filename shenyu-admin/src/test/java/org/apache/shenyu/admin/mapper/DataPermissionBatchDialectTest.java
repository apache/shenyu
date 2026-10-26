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
import org.apache.shenyu.admin.model.entity.DataPermissionDO;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;

import java.io.InputStream;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Oracle-mode regression for batch permission grants.
 */
public class DataPermissionBatchDialectTest {

    @Test
    public void testOracleBatchInsertAndUserLookup() throws Exception {
        DriverManagerDataSource dataSource = new DriverManagerDataSource("jdbc:h2:mem:permission-" + UUID.randomUUID()
                + ";DB_CLOSE_DELAY=-1;MODE=Oracle", "sa", "");
        JdbcTemplate jdbc = new JdbcTemplate(dataSource);
        try {
            jdbc.execute("CREATE TABLE data_permission (id VARCHAR(128) PRIMARY KEY, user_id VARCHAR(128) NOT NULL, data_id VARCHAR(128), data_type INTEGER)");
            Configuration configuration = new Configuration(new Environment("test", new JdbcTransactionFactory(), dataSource));
            configuration.setDatabaseId("oracle");
            ClassPathResource resource = new ClassPathResource("mappers/data-permission-sqlmap.xml");
            try (InputStream input = resource.getInputStream()) {
                new XMLMapperBuilder(input, configuration, resource.toString(), configuration.getSqlFragments()).parse();
            }
            try (SqlSession session = new SqlSessionFactoryBuilder().build(configuration).openSession()) {
                DataPermissionMapper mapper = session.getMapper(DataPermissionMapper.class);
                assertEquals(2, mapper.insertBatch(List.of(DataPermissionDO.buildCreatePermissionDO("data", "first", 0),
                        DataPermissionDO.buildCreatePermissionDO("data", "second", 0))));
                assertEquals(2, mapper.selectUserIds("data", 0).size());
                assertEquals(0, mapper.selectUserIds("data", 1).size());
            }
        } finally {
            jdbc.execute("SHUTDOWN");
        }
    }
}

