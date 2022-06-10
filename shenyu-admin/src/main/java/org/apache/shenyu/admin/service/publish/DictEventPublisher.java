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
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;
import org.apache.shenyu.admin.model.event.dict.BatchDictDeletedEvent;
import org.apache.shenyu.admin.model.event.dict.DictCreatedEvent;
import org.apache.shenyu.admin.model.event.dict.DictUpdatedEvent;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import java.util.Collection;

/**
 * DictEventPublisher.
 */
@Component
public class DictEventPublisher implements AdminDataModelChangedEventPublisher<ShenyuDictDO> {
    
    private final ApplicationEventPublisher publisher;
    
    public DictEventPublisher(final ApplicationEventPublisher publisher) {
        this.publisher = publisher;
    }
    
    /**
     * on selector created.
     *
     * @param data dict
     */
    @Override
    public void onCreated(final ShenyuDictDO data) {
        publish(new DictCreatedEvent(data, SessionUtil.visitorName()));
    }
    
    /**
     * on selector updated.
     *
     * @param selector selector
     * @param before   before selector
     */
    @Override
    public void onUpdated(final ShenyuDictDO selector, final ShenyuDictDO before) {
        publish(new DictUpdatedEvent(selector, before, SessionUtil.visitorName()));
    }
    
    
    /**
     * on dict deleted.
     *
     * @param dict dict
     */
    @Override
    public void onDeleted(final Collection<ShenyuDictDO> dict) {
        publish(new BatchDictDeletedEvent(dict, SessionUtil.visitorName()));
        
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
