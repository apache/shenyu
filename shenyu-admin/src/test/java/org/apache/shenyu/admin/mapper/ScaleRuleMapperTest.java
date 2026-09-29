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

package org.apache.shenyu.admin.mapper;

import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.admin.model.query.ScaleRuleQuery;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

/**
 * Test cases for {@linkplain ScaleRuleMapper}.
 */
public final class ScaleRuleMapperTest extends AbstractSpringIntegrationTest {

    private static final String CPU_RULE_ID = "scale-rule-mapper-test-cpu";

    private static final String MEMORY_RULE_ID = "scale-rule-mapper-test-memory";

    @Autowired
    private ScaleRuleMapper scaleRuleMapper;

    private long initialCount;

    @BeforeEach
    public void insertTestScaleRules() {
        initialCount = scaleRuleMapper.countByQuery(new ScaleRuleQuery());
        scaleRuleMapper.insert(buildScaleRule(CPU_RULE_ID, "cpu_usage"));
        scaleRuleMapper.insert(buildScaleRule(MEMORY_RULE_ID, "memory_usage"));
    }

    @AfterEach
    public void deleteTestScaleRules() {
        scaleRuleMapper.delete(Arrays.asList(CPU_RULE_ID, MEMORY_RULE_ID));
    }

    @Test
    public void countByQueryUsesSameLikePredicateAsSelectByQuery() {
        ScaleRuleQuery query = new ScaleRuleQuery();
        query.setMetricName("cpu");

        List<ScaleRuleDO> selected = scaleRuleMapper.selectByQuery(query);

        assertThat(selected.size(), equalTo(1));
        assertThat(scaleRuleMapper.countByQuery(query), equalTo(1L));
    }

    @Test
    public void countByQueryWithoutMetricNameCountsAllRules() {
        assertThat(scaleRuleMapper.countByQuery(new ScaleRuleQuery()), equalTo(initialCount + 2));
    }

    private ScaleRuleDO buildScaleRule(final String id, final String metricName) {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        return ScaleRuleDO.builder()
                .id(id)
                .dateCreated(now)
                .dateUpdated(now)
                .metricName(metricName)
                .type(0)
                .sort(1)
                .status(1)
                .minimum("0")
                .maximum("100")
                .build();
    }
}
