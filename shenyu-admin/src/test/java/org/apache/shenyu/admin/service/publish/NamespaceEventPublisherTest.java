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

import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test case for {@link NamespaceEventPublisher}.
 */
@ExtendWith(MockitoExtension.class)
class NamespaceEventPublisherTest {

    @Mock
    private ApplicationEventPublisher applicationEventPublisher;

    private NamespaceEventPublisher namespaceEventPublisher;

    @BeforeEach
    void setUp() {
        namespaceEventPublisher = new NamespaceEventPublisher(applicationEventPublisher);
    }

    @Test
    void testPublish() {
        NamespaceVO namespaceVO = new NamespaceVO();
        namespaceVO.setNamespaceId("test-namespace-id");
        AdminDataModelChangedEvent event = new TestNamespaceEvent(namespaceVO);

        namespaceEventPublisher.publish(event);

        verify(applicationEventPublisher, times(1)).publishEvent(event);
    }

    private static class TestNamespaceEvent extends AdminDataModelChangedEvent {
        TestNamespaceEvent(final NamespaceVO namespaceVO) {
            super(namespaceVO, null, EventTypeEnum.NAMESPACE_CREATE, "test-user");
        }
    }
}
