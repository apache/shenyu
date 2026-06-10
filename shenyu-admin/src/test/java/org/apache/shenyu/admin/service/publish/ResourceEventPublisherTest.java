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

import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.event.resource.BatchResourceCreatedEvent;
import org.apache.shenyu.admin.model.event.resource.BatchResourceDeletedEvent;
import org.apache.shenyu.admin.model.event.resource.ResourceChangedEvent;
import org.apache.shenyu.admin.model.event.resource.ResourceCreatedEvent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test case for {@link ResourceEventPublisher}.
 */
@ExtendWith(MockitoExtension.class)
class ResourceEventPublisherTest {

    @Mock
    private ApplicationEventPublisher applicationEventPublisher;

    private ResourceEventPublisher resourceEventPublisher;

    @BeforeEach
    void setUp() {
        resourceEventPublisher = new ResourceEventPublisher(applicationEventPublisher);
    }

    @Test
    void testOnCreatedSingle() {
        ResourceDO resource = buildResourceDO("1", "test-resource");

        resourceEventPublisher.onCreated(resource);

        ArgumentCaptor<ResourceCreatedEvent> captor = ArgumentCaptor.forClass(ResourceCreatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        ResourceCreatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(resource, event.getResource());
    }

    @Test
    void testOnCreatedCollection() {
        ResourceDO resource1 = buildResourceDO("1", "resource1");
        ResourceDO resource2 = buildResourceDO("2", "resource2");
        List<ResourceDO> resources = Arrays.asList(resource1, resource2);

        resourceEventPublisher.onCreated(resources);

        ArgumentCaptor<BatchResourceCreatedEvent> captor = ArgumentCaptor.forClass(BatchResourceCreatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        BatchResourceCreatedEvent event = captor.getValue();
        assertNotNull(event);
    }

    @Test
    void testOnCreatedEmptyCollection() {
        List<ResourceDO> emptyList = Collections.emptyList();

        resourceEventPublisher.onCreated(emptyList);

        verify(applicationEventPublisher, times(1)).publishEvent(any());
    }

    @Test
    void testOnUpdated() {
        ResourceDO before = buildResourceDO("1", "old-resource");
        ResourceDO after = buildResourceDO("1", "new-resource");

        resourceEventPublisher.onUpdated(after, before);

        ArgumentCaptor<ResourceChangedEvent> captor = ArgumentCaptor.forClass(ResourceChangedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        ResourceChangedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(after, event.getAfter());
        assertEquals(before, event.getBefore());
    }

    @Test
    void testOnDeleted() {
        ResourceDO resource1 = buildResourceDO("1", "resource1");
        ResourceDO resource2 = buildResourceDO("2", "resource2");
        List<ResourceDO> resources = Arrays.asList(resource1, resource2);

        resourceEventPublisher.onDeleted(resources);

        ArgumentCaptor<BatchResourceDeletedEvent> captor = ArgumentCaptor.forClass(BatchResourceDeletedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        BatchResourceDeletedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(2, event.getDeletedIds().size());
    }

    @Test
    void testOnDeletedEmptyCollection() {
        List<ResourceDO> emptyList = Collections.emptyList();

        resourceEventPublisher.onDeleted(emptyList);

        verify(applicationEventPublisher, times(1)).publishEvent(any());
    }

    @Test
    void testPublish() {
        ResourceDO resource = buildResourceDO("1", "test-resource");
        ResourceCreatedEvent event = new ResourceCreatedEvent(resource, "test-user");

        resourceEventPublisher.publish(event);

        verify(applicationEventPublisher, times(1)).publishEvent(event);
    }

    private static ResourceDO buildResourceDO(final String id, final String name) {
        ResourceDO resource = new ResourceDO();
        resource.setId(id);
        resource.setName(name);
        resource.setTitle("test-title");
        resource.setUrl("/test/url");
        return resource;
    }
}
