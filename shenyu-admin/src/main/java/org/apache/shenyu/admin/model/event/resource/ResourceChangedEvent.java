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

package org.apache.shenyu.admin.model.event.resource;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;

import java.util.Objects;

/**
 * ResourceChangedEvent.
 */
public class ResourceChangedEvent extends AdminDataModelChangedEvent {
    
    
    /**
     * Create a new {@code ResourceChangedEvent}.operator is unknown.
     *
     * @param source   Current rule state
     * @param before   Before the change rule state
     * @param type     event type
     * @param operator operator
     */
    public ResourceChangedEvent(final ResourceDO source, final ResourceDO before, final EventTypeEnum type, final String operator) {
        super(source, before, type, operator);
    }
    
    @Override
    public String buildContext() {
        final ResourceDO after = (ResourceDO) getAfter();
        if (Objects.isNull(getBefore())) {
            return String.format("the resource [%s] is %s", after.getTitle(), StringUtils.lowerCase(getType().getType().toString()));
        }
        return String.format("the resource [%s] is %s : %s", after.getTitle(), StringUtils.lowerCase(getType().getType().toString()), contrast());
        
    }
    
    private String contrast() {
        final ResourceDO before = (ResourceDO) getBefore();
        Objects.requireNonNull(before);
        final ResourceDO after = (ResourceDO) getAfter();
        Objects.requireNonNull(after);
        if (Objects.equals(before, after)) {
            return "it no change";
        }
        final StringBuilder builder = new StringBuilder();
        if (!Objects.equals(before.getName(), after.getName())) {
            builder.append(String.format("name[%s => %s] ", before.getName(), after.getName()));
        }
        if (!Objects.equals(before.getComponent(), after.getComponent())) {
            builder.append(String.format("component[%s => %s] ", before.getComponent(), after.getComponent()));
        }
        if (!Objects.equals(before.getIcon(), after.getIcon())) {
            builder.append(String.format("match icon[%s => %s] ", before.getIcon(), after.getIcon()));
        }
        if (!Objects.equals(before.getTitle(), after.getTitle())) {
            builder.append(String.format("title[%s => %s] ", before.getTitle(), after.getTitle()));
        }
        if (!Objects.equals(before.getSort(), after.getSort())) {
            builder.append(String.format("sort[%s => %s] ", before.getSort(), after.getSort()));
        }
        if (!Objects.equals(before.getPerms(), after.getPerms())) {
            builder.append(String.format("perms[%s => %s] ", before.getPerms(), after.getPerms()));
        }
        return builder.toString();
    }
    
    @Override
    public String eventName() {
        return "selector";
    }
}
