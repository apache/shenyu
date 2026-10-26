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

import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.exception.ValidFailException;
import org.junit.jupiter.api.Test;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Integration tests for atomic tag deletion.
 */
public class TagDeletionIntegrationTest extends AbstractSpringIntegrationTest {

    @Resource
    private TagService tagService;

    @Resource
    private JdbcTemplate jdbcTemplate;

    @Resource
    private PlatformTransactionManager transactionManager;

    @Test
    public void testDeleteRelationsAndPreserveOtherTags() {
        new TransactionTemplate(transactionManager).executeWithoutResult(status -> {
            insertTag("delete-parent", "/");
            insertTag("delete-child", "delete-parent");
            insertTag("keep-tag", "/");
            assertThrows(ValidFailException.class, () -> tagService.delete(List.of("delete-parent")));
            assertEquals(3, countRelations());
            assertEquals(2, tagService.delete(List.of("delete-parent", "delete-child")));
            assertEquals(1, countRelations());
            assertEquals(1, jdbcTemplate.queryForObject("SELECT COUNT(*) FROM tag WHERE id = 'keep-tag'", Integer.class));
            status.setRollbackOnly();
        });
    }

    @Test
    public void testRollbackRestoresTagsAndRelations() {
        TransactionTemplate transaction = new TransactionTemplate(transactionManager);
        transaction.executeWithoutResult(status -> insertTag("delete-parent", "/"));
        try {
            transaction.executeWithoutResult(status -> {
                assertEquals(1, tagService.delete(List.of("delete-parent")));
                assertEquals(0, countRelations());
                status.setRollbackOnly();
            });
            assertEquals(1, countRelations());
            assertEquals(1, jdbcTemplate.queryForObject("SELECT COUNT(*) FROM tag WHERE id = 'delete-parent'", Integer.class));
        } finally {
            tagService.delete(List.of("delete-parent"));
        }
    }

    private void insertTag(final String id, final String parentId) {
        jdbcTemplate.update("INSERT INTO tag (id, tag_name, tag_desc, parent_tag_id, ext) VALUES (?, ?, '', ?, '{}')", id, id, parentId);
        jdbcTemplate.update("INSERT INTO tag_relation (id, api_id, tag_id) VALUES (?, 'deletion-api', ?)", id, id);
    }

    private int countRelations() {
        return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM tag_relation WHERE api_id = 'deletion-api'", Integer.class);
    }
}

