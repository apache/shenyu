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

package org.dromara.soul.admin.entity;

import org.dromara.soul.admin.dto.RuleDTO;
import org.dromara.soul.admin.entity.base.BaseDOTest;
import org.dromara.soul.common.dto.RuleData;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test for RuleDO.
 *
 * @author lw1243925457
 */
public final class RuleDOTest extends BaseDOTest<RuleDO> {

    @Override
    protected RuleDO getTargetClass() {
        return RuleDO.buildRuleDO(RuleDTO.builder()
                .selectorId("selectorId")
                .matchMode(1)
                .name("name")
                .enabled(true)
                .loged(true)
                .sort(1)
                .handle("handle")
                .build());
    }

    @Test
    void testTransFrom() {
        String id = "id";
        String selectorId = "selectorId";
        Integer matchMode = 1;
        String name = "name";
        Integer sort = 1;
        String handle = "handle";

        RuleDO rule = RuleDO.builder()
                .id(id)
                .selectorId(selectorId)
                .matchMode(matchMode)
                .name(name)
                .enabled(true)
                .loged(true)
                .sort(sort)
                .handle(handle)
                .build();

        final String pluginName = "pluginName";
        RuleData ruleData = RuleDO.transFrom(rule, pluginName, new ArrayList<>(0));

        assertEquals(ruleData.getId(), id);
        assertEquals(ruleData.getSelectorId(), selectorId);
        assertEquals(ruleData.getMatchMode(), matchMode);
        assertEquals(ruleData.getName(), name);
        assertEquals(ruleData.getEnabled(), true);
        assertEquals(ruleData.getLoged(), true);
        assertEquals(ruleData.getSort(), sort);
        assertEquals(ruleData.getHandle(), handle);
        assertEquals(ruleData.getConditionDataList().size(), 0);
    }

}
