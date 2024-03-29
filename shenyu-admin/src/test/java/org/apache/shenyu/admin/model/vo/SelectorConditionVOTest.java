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

package org.apache.shenyu.admin.model.vo;

import org.apache.shenyu.admin.AbstractReflectGetterSetterTest;
import org.apache.shenyu.admin.model.entity.SelectorConditionDO;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for SelectorConditionVO.
 */
public final class SelectorConditionVOTest extends AbstractReflectGetterSetterTest {

    @Override
    protected Class<?> getTargetClass() {
        return SelectorConditionVO.class;
    }

    @Test
    public void testBuildSelectorConditionVO() {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        assertNotNull(SelectorConditionVO.buildSelectorConditionVO(SelectorConditionDO.builder()
                .paramType(ParamTypeEnum.POST.getName())
                .operator(OperatorEnum.MATCH.getAlias())
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .build()));
    }

    @Test
    public void testBuildSelectorConditionVOList() {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        SelectorConditionDO selectorConditionDO = SelectorConditionDO.builder()
                .paramType(ParamTypeEnum.POST.getName())
                .operator(OperatorEnum.MATCH.getAlias())
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .build();
        List<SelectorConditionDO> doList = Lists.newArrayList(selectorConditionDO);
        List<SelectorConditionVO> voList = SelectorConditionVO.buildSelectorConditionVOList(doList);
        assertNotNull(voList);
        assertEquals(voList.size(), doList.size());
    }
}
