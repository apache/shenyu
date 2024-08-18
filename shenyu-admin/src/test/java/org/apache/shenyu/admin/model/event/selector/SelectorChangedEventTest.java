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
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;
import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test case for {@linkplain SelectorChangedEvent}.
 */
public final class SelectorChangedEventTest {
    private SelectorDO before;

    private SelectorDO after;

    @BeforeEach
    public void init() {
        before = buildBeforeSelectorDO();
        after = buildAfterSelectorDO();
    }

    @Test
    void buildContextAndBeforeIsNull() {
        SelectorChangedEvent selectorChangedEvent
                = new SelectorChangedEvent(before, null, EventTypeEnum.SELECTOR_CREATE, "test-operator");

        SelectorDO after = (SelectorDO) selectorChangedEvent.getAfter();
        String expectMsg = String.format("the namespace [%s] selector [%s] is %s", after.getNamespaceId(), after.getName(), StringUtils.lowerCase(selectorChangedEvent.getType().getType().toString()));

        String actualMsg = selectorChangedEvent.buildContext();

        assertEquals(expectMsg, actualMsg);
    }

    @Test
    void buildContextAndBeforeNotNullAndNoChange() {
        SelectorChangedEvent selectorChangedEvent
                = new SelectorChangedEvent(before, before, EventTypeEnum.SELECTOR_CREATE, "test-operator");

        String changeMsg = "it no change";
        SelectorDO after = (SelectorDO) selectorChangedEvent.getAfter();
        String expectMsg = String.format("the namespace [%s] selector [%s] is %s : %s", after.getNamespaceId(), after.getName(),
                StringUtils.lowerCase(selectorChangedEvent.getType().getType().toString()), changeMsg);

        String actualMsg = selectorChangedEvent.buildContext();

        assertEquals(expectMsg, actualMsg);
    }

    @Test
    void buildContextAndBeforeNotNullAndAllChange() {
        SelectorChangedEvent selectorChangedEvent
                = new SelectorChangedEvent(before, after, EventTypeEnum.SELECTOR_CREATE, "test-operator");

        SelectorDO before = (SelectorDO) selectorChangedEvent.getBefore();
        SelectorDO after = (SelectorDO) selectorChangedEvent.getAfter();

        final StringBuilder builder = new StringBuilder();
        builder.append(String.format("name[%s => %s] ", before.getName(), after.getName()));
        builder.append(String.format("handle[%s => %s] ", before.getHandle(), after.getHandle()));
        builder.append(String.format("type[%s => %s] ", before.getType(), after.getType()));
        builder.append(String.format("enable[%s => %s] ", before.getEnabled(), after.getEnabled()));
        builder.append(String.format("sort[%s => %s] ", before.getSort(), after.getSort()));
        builder.append(String.format("loged[%s => %s] ", before.getLoged(), after.getLoged()));

        String changeMsg = builder.toString();
        String expectMsg = String.format("the namespace [%s] selector [%s] is %s : %s", after.getNamespaceId(), after.getName(),
                StringUtils.lowerCase(selectorChangedEvent.getType().getType().toString()), changeMsg);

        String actualMsg = selectorChangedEvent.buildContext();

        assertEquals(expectMsg, actualMsg);
    }

    @Test
    void eventName() {
        SelectorChangedEvent selectorChangedEvent
                = new SelectorChangedEvent(before, null, EventTypeEnum.SELECTOR_CREATE, "test-operator");

        String expectName = "selector";

        String actualName = selectorChangedEvent.eventName();

        assertEquals(expectName, actualName);
    }

    private SelectorDO buildBeforeSelectorDO() {
        SelectorDTO selectorDTO = new SelectorDTO();
        selectorDTO.setId("456");
        selectorDTO.setPluginId("789");
        selectorDTO.setName("kuan");
        selectorDTO.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        selectorDTO.setHandle("[{\"upstreamHost\": \"127.0.0.1\", \"protocol\": \"http://\", \"upstreamUrl\": \"anotherUrl\"}]");

        SelectorDO selectorDO = SelectorDO.buildSelectorDO(selectorDTO);
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        selectorDO.setDateCreated(now);
        selectorDO.setDateUpdated(now);
        selectorDO.setEnabled(true);
        selectorDO.setSort(0);
        selectorDO.setLoged(true);
        return selectorDO;
    }

    private SelectorDO buildAfterSelectorDO() {
        SelectorDTO selectorDTO = new SelectorDTO();
        selectorDTO.setId("456");
        selectorDTO.setPluginId("789");
        selectorDTO.setName("kuan-after");
        selectorDTO.setType(SelectorTypeEnum.CUSTOM_FLOW.getCode());
        selectorDTO.setHandle("[{\"upstreamHost\": \"0.0.0.0\", \"protocol\": \"http://\", \"upstreamUrl\": \"anotherUrl\"}]");

        SelectorDO selectorDO = SelectorDO.buildSelectorDO(selectorDTO);
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        selectorDO.setDateCreated(now);
        selectorDO.setDateUpdated(now);
        selectorDO.setEnabled(false);
        selectorDO.setSort(1);
        selectorDO.setLoged(false);
        return selectorDO;
    }
}
