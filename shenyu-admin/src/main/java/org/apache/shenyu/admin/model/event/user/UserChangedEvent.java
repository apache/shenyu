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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;

import java.util.Objects;

/**
 * UserChangedEvent.
 */
public class UserChangedEvent extends AdminDataModelChangedEvent {
    
    
    /**
     * Create a new {@code UserChangedEvent}.operator is unknown.
     *
     * @param source   Current user state
     * @param before   Before the change user state
     * @param type     event type
     * @param operator operator
     */
    public UserChangedEvent(final DashboardUserDO source, final DashboardUserDO before, final EventTypeEnum type, final String operator) {
        super(source, before, type, operator);
    }
    
    @Override
    public String buildContext() {
        final DashboardUserDO after = (DashboardUserDO) getAfter();
        if (Objects.isNull(getBefore())) {
            return String.format("the selector [%s] is %s", after.getUserName(), StringUtils.lowerCase(getType().getType().toString()));
        }
        return String.format("the selector [%s] is %s : %s", after.getUserName(), StringUtils.lowerCase(getType().getType().toString()), contrast());
        
    }
    
    /**
     * the changed user.
     *
     * @return user
     */
    public DashboardUserDO getChangedUser() {
        return (DashboardUserDO) getSource();
    }
    
    private String contrast() {
        final DashboardUserDO before = (DashboardUserDO) getBefore();
        Objects.requireNonNull(before);
        final DashboardUserDO after = (DashboardUserDO) getAfter();
        Objects.requireNonNull(after);
        if (Objects.equals(before, after)) {
            return "it no change";
        }
        final StringBuilder builder = new StringBuilder();
        if (!Objects.equals(before.getUserName(), after.getUserName())) {
            builder.append(String.format("name[%s => %s] ", before.getUserName(), after.getUserName()));
        }
        if (!Objects.equals(before.getPassword(), after.getPassword())) {
            builder.append("password is changed...");
        }
        if (!Objects.equals(before.getRole(), after.getRole())) {
            builder.append(String.format("role[%s => %s] ", before.getRole(), after.getRole()));
        }
        if (!Objects.equals(before.getEnabled(), after.getEnabled())) {
            builder.append(String.format("enable[%s => %s] ", before.getEnabled(), after.getEnabled()));
        }
        
        return builder.toString();
    }
    
    @Override
    public String eventName() {
        return "user";
    }
}
