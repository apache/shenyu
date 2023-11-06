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
 * test case for {@link DictCreatedEvent}.
 */
public class DictCreatedEventTest {

    private ShenyuDictDO dictDO;

    @BeforeEach
    public void setUp() {
        dictDO = ShenyuDictDO.builder()
                .id("1")
                .type("dictTest")
                .dictCode("DICT_TEST")
                .dictName("test")
                .dictValue("test")
                .desc("dict test desc")
                .sort(0)
                .enabled(true)
                .build();
    }

    @Test
    public void dictCreatedContextTest() {
        DictCreatedEvent dictCreatedEvent = new DictCreatedEvent(dictDO, "test-operator");

        String context = String.format("the dict [%s] is %s", dictDO.getDictName(), StringUtils.lowerCase(EventTypeEnum.DICT_CREATE.getType().toString()));

        assertEquals(context, dictCreatedEvent.buildContext());
    }
}
