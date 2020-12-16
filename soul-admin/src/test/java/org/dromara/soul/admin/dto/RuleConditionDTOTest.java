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

import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

/**
 * Test case for {@link RuleConditionDTO}.
 *
 * @author Jiang Jining
 */
public final class RuleConditionDTOTest {
    
    private RuleConditionDTO ruleConditionDTO;
    
    private RuleConditionDTO ruleConditionDTOConstructor;
    
    @Before
    public void initRuleConditionDTO() {
        ruleConditionDTO = RuleConditionDTO.builder().ruleId("82377625110983143")
                .id("17a60711bc3049998819f84db0720860").operator("Soul-test-user")
                .paramName("Soul-test-param").paramType("Soul-test-type")
                .paramValue("Soul-test-value").build();
        ruleConditionDTOConstructor = new RuleConditionDTO();
        ruleConditionDTOConstructor.setId("9ce0a5a75b6d4bf390fe61ce37c37320");
    }
    
    @Test
    public void testRuleConditionDTO() {
        Assertions.assertNotNull(ruleConditionDTO);
        Assertions.assertNotNull(ruleConditionDTOConstructor);
        Assertions.assertEquals(ruleConditionDTOConstructor.getId(), "9ce0a5a75b6d4bf390fe61ce37c37320");
        Assertions.assertEquals(ruleConditionDTO.getRuleId(), "82377625110983143");
        Assertions.assertEquals(ruleConditionDTO.getId(), "17a60711bc3049998819f84db0720860");
        Assertions.assertEquals(ruleConditionDTO.getOperator(), "Soul-test-user");
        Assertions.assertEquals(ruleConditionDTO.getParamName(), "Soul-test-param");
        Assertions.assertEquals(ruleConditionDTO.getParamValue(), "Soul-test-value");
        Assertions.assertEquals(ruleConditionDTO.getParamType(), "Soul-test-type");
    }
}
