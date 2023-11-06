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

import java.util.Arrays;
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test case for {@link BatchDictChangedEvent}.
 */
public class BatchDictChangedEventTest {

    private ShenyuDictDO one;

    private ShenyuDictDO two;

    @BeforeEach
    public void setUp() {
        one = ShenyuDictDO.builder()
                .id("1")
                .type("dictOneTest")
                .dictCode("DICT_TEST")
                .dictName("one")
                .dictValue("one")
                .desc("dict one test desc")
                .sort(0)
                .enabled(true)
                .build();
        two = ShenyuDictDO.builder()
                .id("2")
                .type("dictTwoTest")
                .dictCode("DICT_TEST")
                .dictName("two")
                .dictValue("two")
                .desc("dict two test desc")
                .sort(0)
                .enabled(true)
                .build();
    }

    @Test
    public void batchChangeDictContextTest() {
        BatchDictChangedEvent batchDictChangedEvent =
                new BatchDictChangedEvent(Arrays.asList(one, two), null, EventTypeEnum.DICT_UPDATE, "test-operator");

        String context = String.format("the shenyu dict[%s] is %s", "one,two", EventTypeEnum.DICT_UPDATE.getType().toString().toLowerCase());

        assertEquals(context, batchDictChangedEvent.buildContext());
    }
}
