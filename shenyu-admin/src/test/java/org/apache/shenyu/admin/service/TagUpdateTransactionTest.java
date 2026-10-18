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
import org.apache.shenyu.admin.model.dto.TagDTO;
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.jdbc.core.JdbcTemplate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
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
        jdbcTemplate.update("INSERT INTO tag (id, tag_name, tag_desc, parent_tag_id, ext) VALUES ('update-grandchild', 'grandchild', '', 'update-child', '{}')");
        jdbcTemplate.update("INSERT INTO tag (id, tag_name, tag_desc, parent_tag_id, ext) VALUES ('update-parent-2', 'parent2', '', '0', '{}')");
    }

    @AfterEach
    public void cleanup() {
        jdbcTemplate.update("DELETE FROM tag WHERE id IN ('update-grandchild', 'update-child', 'update-parent', 'update-parent-2')");
    }

    @Test
    public void testParentFailureRollsBackDescendantUpdates() {
        assertThrows(DataIntegrityViolationException.class, () -> tagService.update(update("x".repeat(129))));
        assertEquals("old", jdbcTemplate.queryForObject("SELECT tag_name FROM tag WHERE id = 'update-parent'", String.class));
        assertEquals("{}", childExt());
    }

    @Test
    public void testExistingCycleIsRejectedWithoutCommittingChanges() {
        jdbcTemplate.update("UPDATE tag SET parent_tag_id = 'update-child' WHERE id = 'update-parent'");
        TagDTO dto = update("new-name");
        dto.setParentTagId("update-child");
        assertThrows(ValidFailException.class, () -> tagService.update(dto));
        assertEquals("old", jdbcTemplate.queryForObject("SELECT tag_name FROM tag WHERE id = 'update-parent'", String.class));
        assertEquals("{}", childExt());
    }

    @ParameterizedTest
    @ValueSource(strings = {"update-parent", "update-child"})
    public void testSelfReferenceAndDescendantReparentingRollBack(final String parentId) {
        TagDTO dto = update("new-name");
        dto.setParentTagId(parentId);
        assertThrows(ValidFailException.class, () -> tagService.update(dto));
        assertEquals("old", jdbcTemplate.queryForObject("SELECT tag_name FROM tag WHERE id = 'update-parent'", String.class));
        assertEquals("0", jdbcTemplate.queryForObject("SELECT parent_tag_id FROM tag WHERE id = 'update-parent'", String.class));
        assertEquals("{}", childExt());
    }

    @Test
    public void testSuccessfulUpdateCommitsParentAndDescendants() {
        assertEquals(1, tagService.update(update("new-name")));
        assertEquals("new-name", jdbcTemplate.queryForObject("SELECT tag_name FROM tag WHERE id = 'update-parent'", String.class));
        assertNotEquals("{}", childExt());
    }

    @Test
    public void testUpdateChildPreservesFullAncestryAndUpdatesExt() {
        TagDTO dto = new TagDTO();
        dto.setId("update-child");
        dto.setParentTagId("update-parent");
        dto.setName("new-child-name");
        dto.setTagDesc("child-desc");
        assertEquals(1, tagService.update(dto));

        String childExt = jdbcTemplate.queryForObject("SELECT ext FROM tag WHERE id = 'update-child'", String.class);
        TagDO.TagExt childTagExt = GsonUtils.getInstance().fromJson(childExt, TagDO.TagExt.class);
        assertNotNull(childTagExt);
        assertNotNull(childTagExt.getParent());
        assertEquals("update-parent", childTagExt.getParent().getId());
        assertEquals("old", childTagExt.getParent().getName());

        String grandchildExt = jdbcTemplate.queryForObject("SELECT ext FROM tag WHERE id = 'update-grandchild'", String.class);
        TagDO.TagExt grandchildTagExt = GsonUtils.getInstance().fromJson(grandchildExt, TagDO.TagExt.class);
        assertNotNull(grandchildTagExt);
        assertNotNull(grandchildTagExt.getParent());
        assertEquals("update-child", grandchildTagExt.getParent().getId());
        assertEquals("new-child-name", grandchildTagExt.getParent().getName());
        assertNotNull(grandchildTagExt.getParent().getParent());
        assertEquals("update-parent", grandchildTagExt.getParent().getParent().getId());
        assertEquals("old", grandchildTagExt.getParent().getParent().getName());
    }

    @Test
    public void testReparentChildTagUpdatesExtAndDescendantsAncestry() {
        TagDTO dto = new TagDTO();
        dto.setId("update-child");
        dto.setParentTagId("update-parent-2");
        dto.setName("child-moved");
        dto.setTagDesc("child-desc");
        assertEquals(1, tagService.update(dto));

        String childExt = jdbcTemplate.queryForObject("SELECT ext FROM tag WHERE id = 'update-child'", String.class);
        TagDO.TagExt childTagExt = GsonUtils.getInstance().fromJson(childExt, TagDO.TagExt.class);
        assertNotNull(childTagExt);
        assertNotNull(childTagExt.getParent());
        assertEquals("update-parent-2", childTagExt.getParent().getId());

        String grandchildExt = jdbcTemplate.queryForObject("SELECT ext FROM tag WHERE id = 'update-grandchild'", String.class);
        TagDO.TagExt grandchildTagExt = GsonUtils.getInstance().fromJson(grandchildExt, TagDO.TagExt.class);
        assertNotNull(grandchildTagExt);
        assertNotNull(grandchildTagExt.getParent());
        assertEquals("update-child", grandchildTagExt.getParent().getId());
        assertNotNull(grandchildTagExt.getParent().getParent());
        assertEquals("update-parent-2", grandchildTagExt.getParent().getParent().getId());
    }

    @Test
    public void testPromoteChildToRootClearsParentInExt() {
        TagDTO dto = new TagDTO();
        dto.setId("update-child");
        dto.setParentTagId("0");
        dto.setName("child-root");
        dto.setTagDesc("child-desc");
        assertEquals(1, tagService.update(dto));

        String childExt = jdbcTemplate.queryForObject("SELECT ext FROM tag WHERE id = 'update-child'", String.class);
        TagDO.TagExt childTagExt = GsonUtils.getInstance().fromJson(childExt, TagDO.TagExt.class);
        assertNotNull(childTagExt);
        assertNull(childTagExt.getParent());

        String grandchildExt = jdbcTemplate.queryForObject("SELECT ext FROM tag WHERE id = 'update-grandchild'", String.class);
        TagDO.TagExt grandchildTagExt = GsonUtils.getInstance().fromJson(grandchildExt, TagDO.TagExt.class);
        assertNotNull(grandchildTagExt);
        assertNotNull(grandchildTagExt.getParent());
        assertEquals("update-child", grandchildTagExt.getParent().getId());
        assertNull(grandchildTagExt.getParent().getParent());
    }

    @Test
    public void testUpdateWithNonExistentParentThrowsValidFailException() {
        TagDTO dto = new TagDTO();
        dto.setId("update-child");
        dto.setParentTagId("non-existent-parent");
        dto.setName("child");
        assertThrows(ValidFailException.class, () -> tagService.update(dto));
    }

    @Test
    public void testUpdateTagWithoutParentTagIdInDtoPreservesRootExt() {
        TagDTO dto = new TagDTO();
        dto.setId("update-parent");
        dto.setName("new-parent-name");
        dto.setTagDesc("desc");
        assertEquals(1, tagService.update(dto));

        String ext = jdbcTemplate.queryForObject("SELECT ext FROM tag WHERE id = 'update-parent'", String.class);
        TagDO.TagExt tagExt = GsonUtils.getInstance().fromJson(ext, TagDO.TagExt.class);
        assertNotNull(tagExt);
        assertNull(tagExt.getParent());
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
