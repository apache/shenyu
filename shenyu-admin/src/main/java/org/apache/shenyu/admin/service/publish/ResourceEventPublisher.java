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
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;
import org.apache.shenyu.admin.model.event.resource.BatchResourceCreatedEvent;
import org.apache.shenyu.admin.model.event.resource.BatchResourceDeletedEvent;
import org.apache.shenyu.admin.model.event.resource.ResourceChangedEvent;
import org.apache.shenyu.admin.model.event.resource.ResourceCreatedEvent;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import java.util.Collection;

/**
 * ResourceEventPublisher.
 */
@Component
public class ResourceEventPublisher implements AdminDataModelChangedEventPublisher<ResourceDO> {
    
    private final ApplicationEventPublisher publisher;
    
    public ResourceEventPublisher(final ApplicationEventPublisher publisher) {
        this.publisher = publisher;
    }
    
    /**
     * on resource created.
     *
     * @param resource resource
     */
    @Override
    public void onCreated(final ResourceDO resource) {
        publish(new ResourceCreatedEvent(resource, SessionUtil.visitorName()));
    }
    
    /**
     * batch on create.
     *
     * @param resource data
     */
    @Override
    public void onCreated(final Collection<ResourceDO> resource) {
        // only system init is batch resource
        publish(new BatchResourceCreatedEvent(resource, "system"));
    }
    
    /**
     * on resource updated.
     *
     * @param resource resource
     * @param before   before resource
     */
    @Override
    public void onUpdated(final ResourceDO resource, final ResourceDO before) {
        publish(new ResourceChangedEvent(resource, before, EventTypeEnum.RULE_DELETE, SessionUtil.visitorName()));
    }
    
    /**
     * on resource delete.
     *
     * @param data data
     */
    @Override
    public void onDeleted(final Collection<ResourceDO> data) {
        publish(new BatchResourceDeletedEvent(data, SessionUtil.visitorName()));
    }
    
    /**
     * event.
     *
     * @param event event.
     */
    @Override
    public void publish(final AdminDataModelChangedEvent event) {
        publisher.publishEvent(event);
    }
}
