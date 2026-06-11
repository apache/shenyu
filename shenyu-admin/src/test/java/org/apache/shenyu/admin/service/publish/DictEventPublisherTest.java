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

package org.apache.shenyu.admin.service.publish;

import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.model.event.dict.BatchDictDeletedEvent;
import org.apache.shenyu.admin.model.event.dict.DictCreatedEvent;
import org.apache.shenyu.admin.model.event.dict.DictUpdatedEvent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test case for {@link DictEventPublisher}.
 */
@ExtendWith(MockitoExtension.class)
class DictEventPublisherTest {

    @Mock
    private ApplicationEventPublisher applicationEventPublisher;

    private DictEventPublisher dictEventPublisher;

    @BeforeEach
    void setUp() {
        dictEventPublisher = new DictEventPublisher(applicationEventPublisher);
    }

    @Test
    void testOnCreated() {
        ShenyuDictDO dict = buildShenyuDictDO("1", "test-type", "test-code");

        dictEventPublisher.onCreated(dict);

        ArgumentCaptor<DictCreatedEvent> captor = ArgumentCaptor.forClass(DictCreatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        DictCreatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(dict, event.getDict());
    }

    @Test
    void testOnUpdated() {
        ShenyuDictDO before = buildShenyuDictDO("1", "test-type", "old-code");
        ShenyuDictDO after = buildShenyuDictDO("1", "test-type", "new-code");

        dictEventPublisher.onUpdated(after, before);

        ArgumentCaptor<DictUpdatedEvent> captor = ArgumentCaptor.forClass(DictUpdatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        DictUpdatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(after, event.getAfter());
        assertEquals(before, event.getBefore());
    }

    @ParameterizedTest
    @MethodSource("provideDictListsForDeleted")
    void testOnDeleted(final List<ShenyuDictDO> dictList, final int expectedSize) {
        dictEventPublisher.onDeleted(dictList);

        ArgumentCaptor<BatchDictDeletedEvent> captor = ArgumentCaptor.forClass(BatchDictDeletedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        BatchDictDeletedEvent event = captor.getValue();
        assertNotNull(event);
        assertNotNull(event.getDeletedIds());
        assertEquals(expectedSize, event.getDeletedIds().size());

        if (expectedSize == 1) {
            assertTrue(event.getDeletedIds().contains("1"));
        }
    }

    private static Stream<Arguments> provideDictListsForDeleted() {
        ShenyuDictDO dict1 = buildShenyuDictDO("1", "test-type", "test-code");
        ShenyuDictDO dict2 = buildShenyuDictDO("1", "test-type", "code1");
        ShenyuDictDO dict3 = buildShenyuDictDO("2", "test-type", "code2");
        ShenyuDictDO dict4 = buildShenyuDictDO("3", "test-type", "code3");

        return Stream.of(
            Arguments.of(Collections.singletonList(dict1), 1),
            Arguments.of(Arrays.asList(dict2, dict3, dict4), 3),
            Arguments.of(Collections.emptyList(), 0)
        );
    }

    @Test
    void testPublish() {
        ShenyuDictDO dict = buildShenyuDictDO("1", "test-type", "test-code");
        DictCreatedEvent event = new DictCreatedEvent(dict, "test-user");

        dictEventPublisher.publish(event);

        verify(applicationEventPublisher, times(1)).publishEvent(event);
    }

    @Test
    void testMultipleOperations() {
        ShenyuDictDO dict1 = buildShenyuDictDO("1", "type1", "code1");
        ShenyuDictDO dict2 = buildShenyuDictDO("2", "type2", "code2");
        ShenyuDictDO dict3 = buildShenyuDictDO("3", "type3", "code3");

        dictEventPublisher.onCreated(dict1);
        dictEventPublisher.onUpdated(dict2, dict1);
        dictEventPublisher.onDeleted(Collections.singletonList(dict3));

        verify(applicationEventPublisher, times(3)).publishEvent(any());
    }

    private static ShenyuDictDO buildShenyuDictDO(final String id, final String type, final String dictCode) {
        ShenyuDictDO dict = new ShenyuDictDO();
        dict.setId(id);
        dict.setType(type);
        dict.setDictCode(dictCode);
        dict.setDictName("test-name");
        dict.setDictValue("test-value");
        dict.setEnabled(true);
        dict.setSort(1);
        return dict;
    }
}
