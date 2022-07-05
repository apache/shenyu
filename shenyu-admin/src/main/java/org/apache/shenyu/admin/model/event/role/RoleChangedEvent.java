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

package org.apache.shenyu.admin.model.event.role;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.RoleDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;

import java.util.Objects;

/**
 * RoleChangedEvent.
 */
public class RoleChangedEvent extends AdminDataModelChangedEvent {
    
    
    /**
     * Create a new {@code RoleChangedEvent}.operator is unknown.
     *
     * @param source   Current role state
     * @param before   Before the change role state
     * @param type     event type
     * @param operator operator
     */
    public RoleChangedEvent(final RoleDO source, final RoleDO before, final EventTypeEnum type, final String operator) {
        super(source, before, type, operator);
    }
    
    @Override
    public String buildContext() {
        final RoleDO after = (RoleDO) getAfter();
        if (Objects.isNull(getBefore())) {
            return String.format("the role [%s] is %s", after.getRoleName(), StringUtils.lowerCase(getType().getType().toString()));
        }
        return String.format("the role [%s] is %s : %s", after.getRoleName(), StringUtils.lowerCase(getType().getType().toString()), contrast());
        
    }
    
    private String contrast() {
        final RoleDO before = (RoleDO) getBefore();
        Objects.requireNonNull(before);
        final RoleDO after = (RoleDO) getAfter();
        Objects.requireNonNull(after);
        if (Objects.equals(before, after)) {
            return "it no change";
        }
        final StringBuilder builder = new StringBuilder();
        if (!Objects.equals(before.getRoleName(), after.getRoleName())) {
            builder.append(String.format("name[%s => %s] ", before.getRoleName(), after.getRoleName()));
        }
        if (!Objects.equals(before.getDescription(), after.getDescription())) {
            builder.append(String.format("disc[%s => %s] ", before.getDescription(), after.getDescription()));
        }
        return builder.toString();
    }
    
    @Override
    public String eventName() {
        return "role";
    }
}
