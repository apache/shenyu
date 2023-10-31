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

package org.apache.shenyu.admin.model.event.dict;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test case for {@link DictUpdatedEvent}.
 */
public class DictUpdatedEventTest {

    private ShenyuDictDO before;

    private ShenyuDictDO after;

    @BeforeEach
    public void setUp() {
        before = ShenyuDictDO.builder()
                .id("1")
                .type("dictTest")
                .dictCode("DICT_TEST")
                .dictName("test")
                .dictValue("test")
                .desc("dict test desc")
                .sort(0)
                .enabled(true)
                .build();

        after = ShenyuDictDO.builder()
                .id("1")
                .type("dictTestAfter")
                .dictCode("DICT_TEST_AFTER")
                .dictName("testAfter")
                .dictValue("testAfter")
                .desc("dict test desc after")
                .sort(1)
                .enabled(false)
                .build();
    }

    @Test
    public void dictUpdatedContextTest() {
        String typeStr = StringUtils.lowerCase(EventTypeEnum.DICT_UPDATE.getType().toString());

        DictUpdatedEvent dictUpdatedNothingEvent = new DictUpdatedEvent(before, before, "test-operator");
        String baseContext = String.format("the dict [%s] is %s : %s", before.getDictName(), typeStr, "it no change");
        assertEquals(baseContext, dictUpdatedNothingEvent.buildContext());

        final StringBuilder contrast = new StringBuilder();
        contrast.append(String.format("name[%s => %s] ", before.getDictName(), after.getDictName()));
        contrast.append(String.format("value[%s => %s] ", before.getDictValue(), after.getDictValue()));
        contrast.append(String.format("desc[%s => %s] ", before.getDesc(), after.getDesc()));
        contrast.append(String.format("type[%s => %s] ", before.getType(), after.getType()));
        contrast.append(String.format("enable[%s => %s] ", before.getEnabled(), after.getEnabled()));
        contrast.append(String.format("sort[%s => %s] ", before.getSort(), after.getSort()));

        String context = String.format("the dict [%s] is %s : %s", after.getDictName(), typeStr, contrast);
        DictUpdatedEvent dictUpdatedEvent = new DictUpdatedEvent(after, before, "test-operator");
        assertEquals(context, dictUpdatedEvent.buildContext());
    }
}
