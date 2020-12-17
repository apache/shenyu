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

import org.dromara.soul.admin.dto.SelectorDTO;
import org.dromara.soul.admin.entity.base.BaseDOTest;
import org.dromara.soul.common.dto.SelectorData;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test for SelectorDO.
 *
 * @author lw1243925457
 */
public final class SelectorDOTest extends BaseDOTest<SelectorDO> {

    @Override
    protected SelectorDO getTargetClass() {
        return SelectorDO.buildSelectorDO(SelectorDTO.builder()
                .type(1)
                .sort(1)
                .enabled(true)
                .loged(true)
                .continued(true)
                .handle("handle")
                .pluginId("pluginId")
                .name("name")
                .build());
    }

    @Test
    public void testTransForm() {
        String id = "id";
        String pluginId = "pluginId";
        String name = "name";
        Integer matchMode = 1;
        Integer type = 1;
        Integer sort = 1;
        String handle = "handle";

        SelectorDO selector = SelectorDO.builder()
                .id(id)
                .pluginId(pluginId)
                .name(name)
                .matchMode(matchMode)
                .type(type)
                .sort(sort)
                .enabled(true)
                .loged(true)
                .continued(true)
                .handle(handle)
                .build();

        final String pluginName = "pluginName";
        SelectorData selectorData = SelectorDO.transFrom(selector, pluginName, new ArrayList<>(0));

        assertEquals(selectorData.getId(), id);
        assertEquals(selectorData.getPluginId(), pluginId);
        assertEquals(selectorData.getName(), name);
        assertEquals(selectorData.getMatchMode(), matchMode);
        assertEquals(selectorData.getType(), type);
        assertEquals(selectorData.getSort(), sort);
        assertEquals(selectorData.getEnabled(), true);
        assertEquals(selectorData.getLoged(), true);
        assertEquals(selectorData.getContinued(), true);
        assertEquals(selectorData.getHandle(), handle);
        assertEquals(selectorData.getConditionList().size(), 0);
    }
}
