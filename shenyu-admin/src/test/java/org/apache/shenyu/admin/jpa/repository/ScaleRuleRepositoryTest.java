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
import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ScaleRuleQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;
import org.springframework.data.domain.Page;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ScaleRuleRepositoryTest extends AbstractSpringIntegrationTest {

    @Resource
    private ScaleRuleRepository scaleRuleRepository;

    @Test
    void selectByQueryWithPaginationAndConditions() {
        scaleRuleRepository.save(buildScaleRule("cpu_usage", 1, 2));
        scaleRuleRepository.save(buildScaleRule("cpu_usage_percent", 1, 1));
        scaleRuleRepository.save(buildScaleRule("memory_usage", 2, 3));

        ScaleRuleQuery query = new ScaleRuleQuery();
        query.setMetricName("cpu");
        query.setPageParameter(new PageParameter(1, 2));
        Page<ScaleRuleDO> page = scaleRuleRepository.selectByQuery(query, PageResultUtils.of(query.getPageParameter()));
        assertEquals(2, page.getTotalElements());

        query.setPageParameter(new PageParameter(2, 2));
        page = scaleRuleRepository.selectByQuery(query, PageResultUtils.of(query.getPageParameter()));
        assertEquals(0, page.getContent().size());

        query.setMetricName(null);
        query.setType(2);
        query.setPageParameter(new PageParameter(1, 10));
        page = scaleRuleRepository.selectByQuery(query, PageResultUtils.of(query.getPageParameter()));
        assertEquals(1, page.getTotalElements());
        assertEquals("memory_usage", page.getContent().get(0).getMetricName());
    }

    @Test
    void selectByQueryOrdersBySortThenDateCreated() {
        scaleRuleRepository.save(buildScaleRule("rule_b", 1, 1));
        scaleRuleRepository.save(buildScaleRule("rule_a", 1, 1));
        Page<ScaleRuleDO> page = scaleRuleRepository.selectByQuery(new ScaleRuleQuery(), PageResultUtils.of(new PageParameter(1, 10)));
        List<ScaleRuleDO> content = page.getContent();
        assertTrue(content.size() >= 2);
        ScaleRuleDO first = content.get(0);
        ScaleRuleDO second = content.get(1);
        assertTrue(first.getSort() < second.getSort()
                || (first.getSort().equals(second.getSort()) && first.getDateCreated().compareTo(second.getDateCreated()) <= 0));
    }

    @Test
    void deleteByIdsReturnsDeletedRowCount() {
        ScaleRuleDO first = scaleRuleRepository.save(buildScaleRule("cpu_usage", 1, 1));
        ScaleRuleDO second = scaleRuleRepository.save(buildScaleRule("memory_usage", 2, 1));
        String missingId = UUIDUtils.getInstance().generateShortUuid();

        int deleted = scaleRuleRepository.deleteByIds(List.of(first.getId(), second.getId(), missingId));
        assertEquals(2, deleted);
        assertTrue(scaleRuleRepository.findById(first.getId()).isEmpty());
    }

    @Test
    void updateLoadedEntityKeepsDateCreated() {
        ScaleRuleDO saved = scaleRuleRepository.save(buildScaleRule("cpu_usage", 1, 1));
        ScaleRuleDO loaded = scaleRuleRepository.findById(saved.getId()).orElseThrow();
        loaded.setMinimum("0.5");
        scaleRuleRepository.save(loaded);

        ScaleRuleDO reloaded = scaleRuleRepository.findById(saved.getId()).orElseThrow();
        assertEquals("0.5", reloaded.getMinimum());
        assertNotNull(reloaded.getDateCreated());
    }

    private ScaleRuleDO buildScaleRule(final String metricName, final Integer type, final Integer sort) {
        ScaleRuleDO scaleRuleDO = new ScaleRuleDO();
        scaleRuleDO.setId(UUIDUtils.getInstance().generateShortUuid());
        scaleRuleDO.setMetricName(metricName);
        scaleRuleDO.setType(type);
        scaleRuleDO.setSort(sort);
        scaleRuleDO.setStatus(1);
        scaleRuleDO.setMinimum("0.1");
        scaleRuleDO.setMaximum("0.9");
        return scaleRuleDO;
    }
}
