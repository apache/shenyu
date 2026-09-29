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
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ShenyuDictQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;
import org.springframework.data.domain.Page;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShenyuDictRepositoryTest extends AbstractSpringIntegrationTest {

    @Resource
    private ShenyuDictRepository shenyuDictRepository;

    @Test
    void selectByQueryWithConditionsAndOrdering() {
        shenyuDictRepository.save(buildDict("enum-type", "CODE_A", "name-a", 2, true));
        shenyuDictRepository.save(buildDict("enum-type", "CODE_B", "name-b", 1, true));
        shenyuDictRepository.save(buildDict("other-type", "CODE_A", "name-a", 1, true));

        ShenyuDictQuery query = new ShenyuDictQuery();
        query.setType("enum-type");
        query.setPageParameter(new PageParameter(1, 10));
        Page<ShenyuDictDO> page = shenyuDictRepository.selectByQuery(query, PageResultUtils.of(query.getPageParameter()));
        assertEquals(2, page.getTotalElements());
        // ORDER BY type, sort, id: sort=1 (CODE_B) comes first
        assertEquals("CODE_B", page.getContent().get(0).getDictCode());

        query.setDictName("name-b");
        page = shenyuDictRepository.selectByQuery(query, PageResultUtils.of(query.getPageParameter()));
        assertEquals(1, page.getTotalElements());
    }

    @Test
    void enabledBatchTogglesOnlyListedIds() {
        ShenyuDictDO first = shenyuDictRepository.save(buildDict("toggle-type", "T1", "t1", 1, true));
        ShenyuDictDO second = shenyuDictRepository.save(buildDict("toggle-type", "T2", "t2", 1, true));

        int updated = shenyuDictRepository.enabled(List.of(first.getId()), false);
        assertEquals(1, updated);
        assertFalse(shenyuDictRepository.findById(first.getId()).orElseThrow().getEnabled());
        assertTrue(shenyuDictRepository.findById(second.getId()).orElseThrow().getEnabled());
    }

    @Test
    void findByTypeInReturnsUnion() {
        shenyuDictRepository.save(buildDict("in-a", "IA", "ia", 1, true));
        shenyuDictRepository.save(buildDict("in-b", "IB", "ib", 1, true));
        shenyuDictRepository.save(buildDict("in-c", "IC", "ic", 1, true));
        assertEquals(2, shenyuDictRepository.findByTypeIn(List.of("in-a", "in-b")).size());
    }

    private ShenyuDictDO buildDict(final String type, final String dictCode, final String dictName, final Integer sort, final Boolean enabled) {
        ShenyuDictDO dictDO = new ShenyuDictDO();
        dictDO.setId(UUIDUtils.getInstance().generateShortUuid());
        dictDO.setType(type);
        dictDO.setDictCode(dictCode);
        dictDO.setDictName(dictName);
        dictDO.setDictValue("v");
        dictDO.setSort(sort);
        dictDO.setEnabled(enabled);
        return dictDO;
    }
}
