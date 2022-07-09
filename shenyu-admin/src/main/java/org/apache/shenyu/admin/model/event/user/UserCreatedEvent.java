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

package org.apache.shenyu.admin.model.event.user;

import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;

/**
 * UserCreatedEvent.
 */
public class UserCreatedEvent extends UserChangedEvent {
    
    
    /**
     * Create a new {@code UserCreatedEvent}.operator is unknown.
     *
     * @param source   Current user state
     * @param operator operator
     */
    public UserCreatedEvent(final DashboardUserDO source, final String operator) {
        super(source, null, EventTypeEnum.USER_CREATE, operator);
    }
    
}
