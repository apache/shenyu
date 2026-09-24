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
import org.apache.shenyu.admin.model.dto.TagDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.jdbc.core.JdbcTemplate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Verify recursive tag updates are committed or rolled back together.
 */
public class TagUpdateTransactionTest extends AbstractSpringIntegrationTest {

    @Resource
    private TagService tagService;

    @Resource
    private JdbcTemplate jdbcTemplate;

    @BeforeEach
    public void setup() {
        jdbcTemplate.update("INSERT INTO tag (id, tag_name, tag_desc, parent_tag_id, ext) VALUES ('update-parent', 'old', '', '0', '{}')");
        jdbcTemplate.update("INSERT INTO tag (id, tag_name, tag_desc, parent_tag_id, ext) VALUES ('update-child', 'child', '', 'update-parent', '{}')");
    }

    @AfterEach
    public void cleanup() {
        jdbcTemplate.update("DELETE FROM tag WHERE id IN ('update-child', 'update-parent')");
    }

    @Test
    public void testParentFailureRollsBackDescendantUpdates() {
        assertThrows(DataIntegrityViolationException.class, () -> tagService.update(update("x".repeat(129))));
        assertEquals("old", jdbcTemplate.queryForObject("SELECT tag_name FROM tag WHERE id = 'update-parent'", String.class));
        assertEquals("{}", childExt());
    }

    @Test
    public void testSuccessfulUpdateCommitsParentAndDescendants() {
        assertEquals(1, tagService.update(update("new-name")));
        assertEquals("new-name", jdbcTemplate.queryForObject("SELECT tag_name FROM tag WHERE id = 'update-parent'", String.class));
        assertNotEquals("{}", childExt());
    }

    private String childExt() {
        return jdbcTemplate.queryForObject("SELECT ext FROM tag WHERE id = 'update-child'", String.class);
    }

    private TagDTO update(final String name) {
        TagDTO dto = new TagDTO();
        dto.setId("update-parent");
        dto.setParentTagId("0");
        dto.setName(name);
        dto.setTagDesc("updated");
        return dto;
    }
}

