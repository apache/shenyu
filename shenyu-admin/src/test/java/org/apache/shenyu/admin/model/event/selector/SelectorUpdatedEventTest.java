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

import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test case for {@linkplain SelectorUpdatedEvent}.
 */
public final class SelectorUpdatedEventTest {
    private SelectorDO selectorDO;

    private SelectorUpdatedEvent selectorUpdatedEvent;

    @BeforeEach
    public void init() {
        selectorDO = buildSelectorDO();

        selectorUpdatedEvent = new SelectorUpdatedEvent(selectorDO, selectorDO, "test-operator");
    }

    @Test
    void getSelector() {
        SelectorDO selector = selectorUpdatedEvent.getSelector();

        assertEquals(selectorDO, selector);
    }

    private SelectorDO buildSelectorDO() {
        SelectorDTO selectorDTO = new SelectorDTO();
        selectorDTO.setId("456");
        selectorDTO.setPluginId("789");
        selectorDTO.setName("kuan");
        selectorDTO.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        selectorDTO.setHandle("[{\"upstreamHost\": \"127.0.0.1\", \"protocol\": \"http://\", \"upstreamUrl\": \"anotherUrl\"}]");
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
}
