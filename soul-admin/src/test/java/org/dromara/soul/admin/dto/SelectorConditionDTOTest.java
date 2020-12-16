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
 * Test case for {@link SelectorConditionDTO}.
 *
 * @author Jiang Jining
 */
public final class SelectorConditionDTOTest {
    
    private SelectorConditionDTO selectorConditionDTO;
    
    @Before
    public void initSelectorConditionDTO() {
        selectorConditionDTO = new SelectorConditionDTO();
        selectorConditionDTO.setId("17a60711bc3049998819f84db0720860");
        selectorConditionDTO.setParamName("Test param");
        selectorConditionDTO.setParamValue("Test value");
        selectorConditionDTO.setOperator("Test operator");
        selectorConditionDTO.setParamType("Test param type");
        selectorConditionDTO.setSelectorId("4c6a60fc0f7f463b9fa332e7132c646a");
    }
    
    @Test
    public void testSelectorConditionDTO() {
        Assertions.assertNotNull(selectorConditionDTO);
        Assertions.assertEquals(selectorConditionDTO.getId(), "17a60711bc3049998819f84db0720860");
        Assertions.assertEquals(selectorConditionDTO.getParamName(), "Test param");
        Assertions.assertEquals(selectorConditionDTO.getParamValue(), "Test value");
        Assertions.assertEquals(selectorConditionDTO.getOperator(), "Test operator");
        Assertions.assertEquals(selectorConditionDTO.getParamType(), "Test param type");
        Assertions.assertEquals(selectorConditionDTO.getSelectorId(), "4c6a60fc0f7f463b9fa332e7132c646a");
    }
}