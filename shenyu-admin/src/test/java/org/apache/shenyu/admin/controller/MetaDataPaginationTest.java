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

import com.github.pagehelper.PageInterceptor;
import org.apache.ibatis.builder.xml.XMLMapperBuilder;
import org.apache.ibatis.mapping.Environment;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import org.apache.ibatis.transaction.jdbc.JdbcTransactionFactory;
import org.apache.shenyu.admin.aspect.PageableAspect;
import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.aop.aspectj.annotation.AspectJProxyFactory;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.io.InputStream;
import java.util.Properties;
import java.util.UUID;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

class MetaDataPaginationTest {

    @ParameterizedTest
    @CsvSource({"1, 0, 2", "3, 4, 1"})
    void reportsTheRowOffsetUsedByPageHelper(final int pageNumber, final int offset, final int returnedRows) throws Exception {
        DriverManagerDataSource dataSource = new DriverManagerDataSource(
                "jdbc:h2:mem:metadata-pagination-" + UUID.randomUUID() + ";DB_CLOSE_DELAY=-1;MODE=MySQL", "sa", "");
        JdbcTemplate jdbc = new JdbcTemplate(dataSource);
        try {
            new ResourceDatabasePopulator(new ClassPathResource("sql-script/h2/schema.sql")).execute(dataSource);
            for (int i = 1; i <= 5; i++) {
                jdbc.update("INSERT INTO meta_data (id, app_name, path, rpc_type, namespace_id) VALUES (?, ?, ?, 'http', ?)",
                        String.valueOf(i), "app", "/path-" + i, SYS_DEFAULT_NAMESPACE_ID);
            }
            final Configuration configuration = new Configuration(new Environment("metadata-pagination", new JdbcTransactionFactory(), dataSource));
            final PageInterceptor interceptor = new PageInterceptor();
            Properties properties = new Properties();
            properties.setProperty("helperDialect", "h2");
            properties.setProperty("reasonable", "false");
            properties.setProperty("pageSizeZero", "true");
            interceptor.setProperties(properties);
            configuration.addInterceptor(interceptor);
            String mapperResource = "mappers/meta-data-sqlmap.xml";
            try (InputStream stream = new ClassPathResource(mapperResource).getInputStream()) {
                new XMLMapperBuilder(stream, configuration, mapperResource, configuration.getSqlFragments()).parse();
            }
            try (SqlSession session = new SqlSessionFactoryBuilder().build(configuration).openSession()) {
                MetaDataMapper mapper = session.getMapper(MetaDataMapper.class);
                AspectJProxyFactory proxy = new AspectJProxyFactory(new MetaDataServiceImpl(mapper, null, null));
                proxy.addAspect(new PageableAspect());
                MetaDataService service = proxy.getProxy();
                MockMvc mvc = MockMvcBuilders.standaloneSetup(new MetaDataController(service)).build();

                mvc.perform(MockMvcRequestBuilders.get("/meta-data/queryList")
                                .param("currentPage", String.valueOf(pageNumber))
                                .param("pageSize", "2")
                                .param("namespaceId", SYS_DEFAULT_NAMESPACE_ID))
                        .andExpect(status().isOk())
                        .andExpect(jsonPath("$.data.page.currentPage").value(pageNumber))
                        .andExpect(jsonPath("$.data.page.pageSize").value(2))
                        .andExpect(jsonPath("$.data.page.totalPage").value(3))
                        .andExpect(jsonPath("$.data.page.totalCount").value(5))
                        .andExpect(jsonPath("$.data.dataList.length()").value(returnedRows))
                        .andExpect(jsonPath("$.data.page.offset").value(offset));
            }
        } finally {
            jdbc.execute("SHUTDOWN");
        }
    }
}
