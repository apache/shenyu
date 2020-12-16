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

package org.dromara.soul.admin.dto;

import org.assertj.core.util.Lists;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

import java.util.ArrayList;
import java.util.List;

/**
 * Test case for {@link RuleDTO}.
 *
 * @author Jiang Jining
 */
public final class RuleDTOTest {
    
    private RuleDTO ruleDTOBuilder;
    
    private RuleDTO ruleDTOConstructor;
    
    @Before
    public void initRuleDTO() {
        ArrayList<RuleConditionDTO> ruleConditionDTOArrayList = Lists.newArrayList(new RuleConditionDTO());
        ruleDTOBuilder = RuleDTO.builder().enabled(false).handle("Handle-rule-test")
                .id("dc2ac6fa7b544fd2a0066a6af796c949")
                .loged(false).matchMode(1).name("Test-rule")
                .selectorId("0fd61e49886348cc8e7484e3e34560ed")
                .sort(2).ruleConditions(ruleConditionDTOArrayList)
                .build();
        ruleDTOConstructor = new RuleDTO();
        ruleDTOConstructor.setHandle("Handle-rule-test");
        ruleDTOConstructor.setEnabled(false);
        ruleDTOConstructor.setId("dc2ac6fa7b544fd2a0066a6af796c949");
        ruleDTOConstructor.setLoged(false);
        ruleDTOConstructor.setMatchMode(1);
        ruleDTOConstructor.setName("Test-rule");
        ruleDTOConstructor.setSelectorId("0fd61e49886348cc8e7484e3e34560ed");
        ruleDTOConstructor.setSort(2);
        ruleDTOConstructor.setRuleConditions(ruleConditionDTOArrayList);
    }
    
    @Test
    public void testRuleDTO() {
        Assertions.assertNotNull(ruleDTOBuilder);
        Assertions.assertNotNull(ruleDTOConstructor);
        Assertions.assertEquals(ruleDTOConstructor, ruleDTOBuilder);
        Assertions.assertFalse(ruleDTOBuilder.getEnabled());
        Assertions.assertEquals(ruleDTOBuilder.getHandle(), "Handle-rule-test");
        Assertions.assertEquals(ruleDTOBuilder.getId(), "dc2ac6fa7b544fd2a0066a6af796c949");
        Assertions.assertFalse(ruleDTOBuilder.getLoged());
        Assertions.assertEquals(ruleDTOBuilder.getMatchMode(), 1);
        Assertions.assertEquals(ruleDTOBuilder.getName(), "Test-rule");
        Assertions.assertEquals(ruleDTOBuilder.getSelectorId(), "0fd61e49886348cc8e7484e3e34560ed");
        Assertions.assertEquals(ruleDTOBuilder.getSort(), 2);
        List<RuleConditionDTO> ruleConditions = ruleDTOBuilder.getRuleConditions();
        Assertions.assertNotNull(ruleConditions);
        Assertions.assertEquals(ruleConditions.size(), 1);
        ruleDTOBuilder.setEnabled(true);
        Assertions.assertTrue(ruleDTOBuilder.getEnabled());
        ruleDTOBuilder.setLoged(true);
        Assertions.assertTrue(ruleDTOBuilder.getLoged());
    }
}
