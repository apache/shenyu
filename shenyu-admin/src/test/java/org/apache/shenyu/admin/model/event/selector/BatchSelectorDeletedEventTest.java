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

package org.apache.shenyu.admin.model.event.selector;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.BaseDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.ListUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;

/**
 * Test case for {@linkplain BatchSelectorDeletedEvent}.
 */
public final class BatchSelectorDeletedEventTest {
    private SelectorDO selectorDO;

    private List<SelectorDO> selectorDOList;

    private PluginDO pluginDO;

    private List<PluginDO> pluginDOList;

    private BatchSelectorDeletedEvent batchSelectorDeletedEvent;

    @BeforeEach
    public void init() {
        selectorDO = buildSelectorDO();
        selectorDOList = Collections.singletonList(selectorDO);

        pluginDO = buildPluginDO();
        pluginDOList = Collections.singletonList(pluginDO);

        batchSelectorDeletedEvent = new BatchSelectorDeletedEvent(selectorDOList, "test-operator", pluginDOList);
    }

    @Test
    void buildContext() {
        String expectMsg = String.format("the namespace [%s] selector[%s] is %s", selectorDO.getNamespaceId(),
                selectorDO.getName(), StringUtils.lowerCase(batchSelectorDeletedEvent.getType().getType().toString()));

        String actualMsg = batchSelectorDeletedEvent.buildContext();

        assertEquals(expectMsg, actualMsg);
    }

    @Test
    void getSelectors() {
        List<SelectorDO> selectors = batchSelectorDeletedEvent.getSelectors();

        assertNotNull(selectors);
        assertEquals(selectors.size(), selectorDOList.size());
    }

    @Test
    void findPluginBySelectorId() {
        PluginDO findPluginDO = batchSelectorDeletedEvent.findPluginBySelectorId(selectorDO.getId());

        assertEquals(pluginDO.getId(), findPluginDO.getId());
    }

    @Test
    void getPlugins() {
        List<PluginDO> plugins = batchSelectorDeletedEvent.getPlugins();

        assertArrayEquals(pluginDOList.toArray(new PluginDO[0]), plugins.toArray(new PluginDO[0]));
    }

    @Test
    void getDeletedIds() {
        List<String> expectDeleteIds = ListUtil.map(selectorDOList, BaseDO::getId);

        List<String> actualDeleteIds = batchSelectorDeletedEvent.getDeletedIds();

        assertArrayEquals(expectDeleteIds.toArray(new String[0]), actualDeleteIds.toArray(new String[0]));
    }

    private SelectorDO buildSelectorDO() {
        SelectorDTO selectorDTO = new SelectorDTO();
        selectorDTO.setId("456");
        selectorDTO.setPluginId("789");
        selectorDTO.setName("kuan");
        selectorDTO.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        selectorDTO.setHandle("[{\"upstreamHost\": \"127.0.0.1\", \"protocol\": \"http://\", \"upstreamUrl\": \"anotherUrl\"}]");
        selectorDTO.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        SelectorConditionDTO selectorConditionDTO1 = new SelectorConditionDTO();
        selectorConditionDTO1.setId("111");
        selectorConditionDTO1.setSelectorId("456");
        SelectorConditionDTO selectorConditionDTO2 = new SelectorConditionDTO();
        selectorConditionDTO2.setId("222");
        selectorConditionDTO2.setSelectorId("456");
        selectorDTO.setSelectorConditions(Arrays.asList(selectorConditionDTO1, selectorConditionDTO2));
        SelectorDO selectorDO = SelectorDO.buildSelectorDO(selectorDTO);
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        selectorDO.setDateCreated(now);
        selectorDO.setDateUpdated(now);
        return selectorDO;
    }

    private PluginDO buildPluginDO() {
        PluginDO pluginDO = new PluginDO();
        pluginDO.setName("test");
        pluginDO.setId("789");
        return pluginDO;
    }
}
