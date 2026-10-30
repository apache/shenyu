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

import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.service.impl.SelectorServiceImpl;
import org.apache.shenyu.admin.service.publish.SelectorEventPublisher;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.junit.jupiter.api.Test;
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

import java.util.UUID;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

/**
 * Independent Admin service instances serialize registration through a database row lock.
 */
class SelectorRegistrationConcurrencyTest {

    @Test
    void concurrentAdminsCreateOneSelectorPerNamespace() throws Exception {
        DriverManagerDataSource dataSource = new DriverManagerDataSource(
                "jdbc:h2:mem:selector-" + UUID.randomUUID() + ";MODE=MySQL;DB_CLOSE_DELAY=-1;LOCK_TIMEOUT=5000", "sa", "");
        new ResourceDatabasePopulator(new ClassPathResource("sql-script/h2/schema.sql")).execute(dataSource);
        final JdbcTemplate jdbc = new JdbcTemplate(dataSource);
        SqlSessionFactoryBean factory = new SqlSessionFactoryBean();
        factory.setDataSource(dataSource);
        factory.setMapperLocations(new ClassPathResource("mappers/plugin-sqlmap.xml"), new ClassPathResource("mappers/selector-sqlmap.xml"));
        SqlSessionTemplate sessions = new SqlSessionTemplate(factory.getObject());
        PluginMapper plugins = sessions.getMapper(PluginMapper.class);
        SelectorMapper selectors = sessions.getMapper(SelectorMapper.class);
        DataSourceTransactionManager transactions = new DataSourceTransactionManager(dataSource);
        final SelectorService first = service(selectors, plugins, transactions);
        final SelectorService second = service(selectors, plugins, transactions);
        ExecutorService workers = Executors.newFixedThreadPool(2);
        try {
            for (int iteration = 0; iteration < 8; iteration++) {
                final String path = "/concurrent-" + iteration;
                final CyclicBarrier start = new CyclicBarrier(2);
                Future<String> one = workers.submit(() -> register(first, path, start));
                Future<String> two = workers.submit(() -> register(second, path, start));
                assertEquals(one.get(10, TimeUnit.SECONDS), two.get(10, TimeUnit.SECONDS));
                assertEquals(1, jdbc.queryForObject("SELECT COUNT(*) FROM selector WHERE selector_name = ? AND namespace_id = ?",
                        Integer.class, path, SYS_DEFAULT_NAMESPACE_ID));
            }
            MetaDataRegisterDTO existing = new MetaDataRegisterDTO();
            existing.setContextPath("/concurrent-0");
            existing.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
            String expectedId = jdbc.queryForObject("SELECT id FROM selector WHERE selector_name = ? AND namespace_id = ?",
                    String.class, "/concurrent-0", SYS_DEFAULT_NAMESPACE_ID);
            assertEquals(expectedId, first.registerDefault(existing, "divide", "[]"));
            assertEquals(1, jdbc.queryForObject("SELECT COUNT(*) FROM selector WHERE selector_name = ? AND namespace_id = ?",
                    Integer.class, "/concurrent-0", SYS_DEFAULT_NAMESPACE_ID));
        } finally {
            workers.shutdownNow();
            jdbc.execute("SHUTDOWN");
        }
    }

    private String register(final SelectorService service, final String path, final CyclicBarrier start) throws Exception {
        MetaDataRegisterDTO metadata = new MetaDataRegisterDTO();
        metadata.setContextPath(path);
        metadata.setAppName("concurrent-app");
        metadata.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        start.await(5, TimeUnit.SECONDS);
        return service.registerDefault(metadata, "divide", "[]");
    }

    private SelectorService service(final SelectorMapper selectors, final PluginMapper plugins,
                                    final DataSourceTransactionManager transactions) {
        SelectorServiceImpl target = new SelectorServiceImpl(selectors, mock(SelectorConditionMapper.class), plugins,
                mock(ApplicationEventPublisher.class), mock(DiscoveryMapper.class), mock(DiscoveryHandlerMapper.class),
                mock(DiscoveryRelMapper.class), mock(DiscoveryUpstreamMapper.class), mock(DiscoveryProcessorHolder.class),
                mock(SelectorEventPublisher.class), mock(DataPermissionChecker.class));
        ProxyFactory proxy = new ProxyFactory(target);
        proxy.addAdvice(new TransactionInterceptor(transactions, new AnnotationTransactionAttributeSource()));
        return (SelectorService) proxy.getProxy();
    }
}

