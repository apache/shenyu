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
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

/**
 * NamespaceEventPublisher.
 */
@Component
public class NamespaceEventPublisher implements AdminDataModelChangedEventPublisher<NamespaceVO> {

    private final ApplicationEventPublisher publisher;

    public NamespaceEventPublisher(final ApplicationEventPublisher publisher) {
        this.publisher = publisher;
    }

    @Override
    public void publish(final AdminDataModelChangedEvent event) {
        publisher.publishEvent(event);
    }
}
