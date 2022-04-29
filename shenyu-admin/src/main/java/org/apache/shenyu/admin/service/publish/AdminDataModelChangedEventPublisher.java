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

import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;
import org.apache.shenyu.admin.model.event.BatchChangedEvent;
import org.apache.shenyu.admin.utils.SessionUtil;

import java.util.Collection;

/**
 * ModelDataEventPublisher.
 */
public interface AdminDataModelChangedEventPublisher<T> {
    
    /**
     * on  created.
     *
     * @param data data
     */
    default void onCreated(final T data) {
        publish(new AdminDataModelChangedEvent(data, null, EventTypeEnum.CREATE, SessionUtil.visitorName()));
    }
    
    /**
     * on  created.
     *
     * @param data data
     */
    default void onCreated(final Collection<T> data) {
        publish(new BatchChangedEvent(data, null, EventTypeEnum.CREATE, SessionUtil.visitorName()));
    }
    
    /**
     * on data updated.
     *
     * @param data   data
     * @param before before data
     */
    default void onUpdated(final T data, final T before) {
        publish(new AdminDataModelChangedEvent(data, before, EventTypeEnum.UPDATE, SessionUtil.visitorName()));
    }
    
    /**
     * on data updated.
     *
     * @param data   data
     * @param before before data
     */
    default void onUpdated(final Collection<T> data, final Collection<T> before) {
        publish(new BatchChangedEvent(data, before, EventTypeEnum.UPDATE, SessionUtil.visitorName()));
    }
    
    /**
     * on data deleted.
     *
     * @param data data
     */
    default void onDeleted(final T data) {
        publish(new AdminDataModelChangedEvent(data, null, EventTypeEnum.DELETE, SessionUtil.visitorName()));
    }
    
    
    /**
     * on data deleted.
     *
     * @param data data
     */
    default void onDeleted(final Collection<T> data) {
        publish(new BatchChangedEvent(data, null, EventTypeEnum.DELETE, SessionUtil.visitorName()));
    }
    
    /**
     * event.
     *
     * @param event event.
     */
    void publish(AdminDataModelChangedEvent event);
}
