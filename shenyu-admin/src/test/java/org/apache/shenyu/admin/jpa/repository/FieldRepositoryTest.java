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

package org.apache.shenyu.admin.jpa.repository;

import jakarta.annotation.Resource;
import jakarta.persistence.EntityManager;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.FieldDO;
import org.apache.shenyu.admin.model.query.FieldQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;
import org.springframework.data.domain.Pageable;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FieldRepositoryTest extends AbstractSpringIntegrationTest {

    @Resource
    private FieldRepository fieldRepository;

    @Resource
    private EntityManager entityManager;

    @Test
    @Transactional
    void crudWithRequiredFlagRoundTrip() {
        FieldDO fieldDO = buildField("6138-model-a", "6138-field-a", Boolean.TRUE);
        fieldRepository.save(fieldDO);

        FieldDO loaded = fieldRepository.findById(fieldDO.getId()).orElse(null);
        assertEquals(fieldDO.getName(), loaded.getName());
        assertTrue(loaded.getRequired());

        loaded.setFieldDesc("changed desc");
        fieldRepository.saveAndFlush(loaded);
        entityManager.clear();

        FieldDO updated = fieldRepository.findById(fieldDO.getId()).orElse(null);
        assertEquals("changed desc", updated.getFieldDesc());
        assertTrue(updated.getRequired());
        assertEquals(fieldDO.getName(), updated.getName());

        fieldRepository.deleteAllByIdInBatch(List.of(fieldDO.getId()));
        // deleteAllByIdInBatch is a bulk DELETE that bypasses the persistence context; clear it before re-reading
        entityManager.clear();
        assertFalse(fieldRepository.findById(fieldDO.getId()).isPresent());
    }

    @Test
    @Transactional
    void pageByQueryAppliesOnlyNonNullConditions() {
        FieldDO first = buildField("6138-model-a", "6138-field-a", Boolean.TRUE);
        FieldDO second = buildField("6138-model-b", "6138-field-b", Boolean.FALSE);
        fieldRepository.save(first);
        fieldRepository.save(second);

        FieldQuery byName = new FieldQuery();
        byName.setName(first.getName());
        List<FieldDO> result = fieldRepository.pageByQuery(byName, Pageable.unpaged()).getContent();
        assertEquals(1, result.size());
        assertEquals(first.getId(), result.get(0).getId());

        FieldQuery byDesc = new FieldQuery();
        byDesc.setFieldDesc("6138 field desc");
        assertEquals(2, fieldRepository.pageByQuery(byDesc, Pageable.unpaged()).getTotalElements());
    }

    private FieldDO buildField(final String modelId, final String selfModelId, final Boolean required) {
        return FieldDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .modelId(modelId)
                .selfModelId(selfModelId)
                .name("name-" + selfModelId)
                .fieldDesc("6138 field desc")
                .required(required)
                .ext("{}")
                .build();
    }
}
