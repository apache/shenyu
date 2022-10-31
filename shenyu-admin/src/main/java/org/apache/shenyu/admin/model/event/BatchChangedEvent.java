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

package org.apache.shenyu.admin.model.event;

import org.apache.shenyu.admin.model.enums.EventTypeEnum;

import java.util.Collection;
import java.util.Objects;

/**
 * BatchChangedEvent.
 */
public class BatchChangedEvent extends AdminDataModelChangedEvent {
    
    
    /**
     * Create a new {@code PluginChangedEvent}.operator is unknown.
     *
     * @param source   Current plugin state
     * @param before   Before the change plugin state
     * @param type     event type
     * @param operator operator
     */
    public BatchChangedEvent(final Collection<?> source, final Collection<?> before, final EventTypeEnum type, final String operator) {
        super(source, before, type, operator);
    }
    
    /**
     * before plugin snapshot.
     *
     * @return snapshot
     */
    @Override
    public String beforeSnapshot() {
        // format plugin data
        return Objects.toString(getBefore(), "before plugin unknown");
    }
    
    /**
     * after plugin snapshot.
     *
     * @return snapshot
     */
    @Override
    public String afterSnapshot() {
        // format plugin data
        return Objects.toString(getAfter(), "after plugin unknown");
    }
    
    @Override
    public String eventName() {
        return "plugin";
    }
}
