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
import org.apache.shenyu.admin.model.entity.DetailDO;
import org.apache.shenyu.admin.model.query.DetailQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;
import org.springframework.data.domain.Pageable;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DetailRepositoryTest extends AbstractSpringIntegrationTest {

    @Resource
    private DetailRepository detailRepository;

    @Resource
    private EntityManager entityManager;

    @Test
    @Transactional
    void crudWithExampleFlagRoundTrip() {
        DetailDO detailDO = buildDetail("6138-field-a", Boolean.TRUE);
        detailRepository.save(detailDO);

        DetailDO loaded = detailRepository.findById(detailDO.getId()).orElse(null);
        assertEquals(detailDO.getFieldValue(), loaded.getFieldValue());
        assertTrue(loaded.getExample());

        loaded.setFieldValue("changed value");
        detailRepository.saveAndFlush(loaded);
        entityManager.clear();

        DetailDO updated = detailRepository.findById(detailDO.getId()).orElse(null);
        assertEquals("changed value", updated.getFieldValue());
        assertTrue(updated.getExample());
        assertEquals(detailDO.getValueDesc(), updated.getValueDesc());

        detailRepository.deleteAllByIdInBatch(List.of(detailDO.getId()));
        // deleteAllByIdInBatch is a bulk DELETE that bypasses the persistence context; clear it before re-reading
        entityManager.clear();
        assertFalse(detailRepository.findById(detailDO.getId()).isPresent());
    }

    @Test
    @Transactional
    void pageByQueryAppliesOnlyNonNullConditions() {
        DetailDO first = buildDetail("6138-field-a", Boolean.TRUE);
        DetailDO second = buildDetail("6138-field-b", Boolean.FALSE);
        detailRepository.save(first);
        detailRepository.save(second);

        DetailQuery byFieldValue = new DetailQuery(first.getFieldValue(), null, null);
        List<DetailDO> result = detailRepository.pageByQuery(byFieldValue, Pageable.unpaged()).getContent();
        assertEquals(1, result.size());
        assertEquals(first.getId(), result.get(0).getId());

        DetailQuery byValueDesc = new DetailQuery(null, "6138 detail desc", null);
        assertEquals(2, detailRepository.pageByQuery(byValueDesc, Pageable.unpaged()).getTotalElements());

        DetailQuery all = new DetailQuery(null, null, null);
        assertEquals(2, detailRepository.pageByQuery(all, Pageable.unpaged()).getTotalElements());
    }

    private DetailDO buildDetail(final String fieldId, final Boolean example) {
        return DetailDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .fieldId(fieldId)
                .example(example)
                .fieldValue("value of " + fieldId)
                .valueDesc("6138 detail desc")
                .build();
    }
}
