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

import org.apache.ibatis.builder.xml.XMLMapperBuilder;
import org.apache.ibatis.mapping.Environment;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import org.apache.ibatis.transaction.jdbc.JdbcTransactionFactory;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.NamespaceUserRelMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.dto.NamespaceDTO;
import org.apache.shenyu.admin.model.event.namespace.NamespaceCreatedEvent;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.service.impl.NamespaceServiceImpl;
import org.apache.shenyu.admin.service.impl.NamespaceUserServiceImpl;
import org.apache.shenyu.admin.service.publish.NamespaceEventPublisher;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shiro.authz.UnauthorizedException;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.ThreadContext;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;

import java.io.InputStream;
import java.util.Objects;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

/**
 * Membership boundary over real namespace persistence: a caller must not be able to create a
 * namespace that collides with an existing namespace id and thereby gain membership in it.
 */
public final class NamespaceMutationPersistenceTest {

    private static final String[] MAPPER_RESOURCES = {
        "mappers/namespace-sqlmap.xml",
        "mappers/namespace-user-rel-sqlmap.xml",
    };

    private JdbcTemplate jdbc;

    private SqlSession session;

    @BeforeEach
    public void setUp() throws Exception {
        final DriverManagerDataSource dataSource = new DriverManagerDataSource(
                "jdbc:h2:mem:namespace-mutation-" + UUID.randomUUID() + ";DB_CLOSE_DELAY=-1;MODE=MySQL", "sa", "");
        jdbc = new JdbcTemplate(dataSource);
        new ResourceDatabasePopulator(new ClassPathResource("sql-script/h2/schema.sql")).execute(dataSource);
        session = openSession(dataSource);
        ThreadContext.bind(mock(Subject.class));
        SessionUtil.setLocalVisitor(UserInfo.builder().userId("user-1").userName("user-1").build());
    }

    @AfterEach
    public void tearDown() {
        ThreadContext.unbindSubject();
        SessionUtil.setLocalVisitor(null);
        if (Objects.nonNull(session)) {
            session.close();
        }
        if (Objects.nonNull(jdbc)) {
            jdbc.execute("SHUTDOWN");
        }
    }

    @Test
    public void testCollidingCreateCannotGrantMembershipInForeignNamespace() {
        jdbc.update("INSERT INTO namespace (id, namespace_id, name) VALUES (?, ?, ?)",
                "victim-id", "victim-ns", "victim");
        final NamespaceUserRelMapper relMapper = session.getMapper(NamespaceUserRelMapper.class);
        final NamespaceServiceImpl namespaceService = newService(session.getMapper(NamespaceMapper.class), relMapper);

        // a non-member cannot update the foreign namespace
        assertThrows(UnauthorizedException.class,
                () -> namespaceService.createOrUpdate(updateDTO("victim-ns")));
        // a colliding create is rejected before it can grant membership
        assertThrows(ShenyuAdminException.class,
                () -> namespaceService.createOrUpdate(createDTO("victim-ns")));
        assertEquals(1, jdbc.queryForObject(
                "SELECT COUNT(*) FROM namespace WHERE namespace_id = ?", Integer.class, "victim-ns").intValue());
        assertNull(relMapper.selectByNamespaceIdAndUserId("victim-ns", "user-1"));
        // and the foreign namespace stays out of reach for that caller
        assertThrows(UnauthorizedException.class,
                () -> namespaceService.createOrUpdate(updateDTO("victim-ns")));
    }

    @Test
    public void testCreateGrantsMembershipOnlyForTheCreatedNamespace() {
        final NamespaceUserRelMapper relMapper = session.getMapper(NamespaceUserRelMapper.class);
        final NamespaceServiceImpl namespaceService = newService(session.getMapper(NamespaceMapper.class), relMapper);

        final NamespaceVO created = namespaceService.createOrUpdate(createDTO("fresh-ns"));

        assertNotNull(created);
        assertNotNull(relMapper.selectByNamespaceIdAndUserId("fresh-ns", "user-1"));
        assertNotNull(namespaceService.createOrUpdate(updateDTO("fresh-ns")));
    }

    private NamespaceServiceImpl newService(final NamespaceMapper namespaceMapper,
                                            final NamespaceUserRelMapper relMapper) {
        final NamespaceUserServiceImpl namespaceUserService = new NamespaceUserServiceImpl(relMapper);
        final NamespaceEventPublisher publisher = new NamespaceEventPublisher(event -> {
            if (event instanceof NamespaceCreatedEvent) {
                namespaceUserService.onNamespaceCreated((NamespaceCreatedEvent) event);
            }
        });
        return new NamespaceServiceImpl(namespaceMapper, namespaceUserService, publisher,
                mock(NamespacePluginRelMapper.class), mock(SelectorMapper.class), mock(RuleMapper.class),
                mock(AuthPathMapper.class), mock(MetaDataMapper.class), mock(DiscoveryMapper.class),
                mock(AppAuthMapper.class));
    }

    private SqlSession openSession(final DriverManagerDataSource dataSource) throws Exception {
        final Configuration configuration = new Configuration(
                new Environment("test", new JdbcTransactionFactory(), dataSource));
        configuration.setMapUnderscoreToCamelCase(true);
        for (String mapperResource : MAPPER_RESOURCES) {
            try (InputStream input = new ClassPathResource(mapperResource).getInputStream()) {
                new XMLMapperBuilder(input, configuration, mapperResource, configuration.getSqlFragments()).parse();
            }
        }
        return new SqlSessionFactoryBuilder().build(configuration).openSession(true);
    }

    private NamespaceDTO createDTO(final String namespaceId) {
        return namespaceDTO(null, namespaceId);
    }

    private NamespaceDTO updateDTO(final String namespaceId) {
        return namespaceDTO("row-id", namespaceId);
    }

    private NamespaceDTO namespaceDTO(final String id, final String namespaceId) {
        final NamespaceDTO dto = new NamespaceDTO();
        dto.setId(id);
        dto.setNamespaceId(namespaceId);
        dto.setName("name");
        dto.setDescription("description");
        return dto;
    }
}
