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

package org.apache.shenyu.admin.model.event.rule;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;

import java.util.Objects;

/**
 * SelectorChangedEvent.
 */
public class RuleChangedEvent extends AdminDataModelChangedEvent {
    
    
    /**
     * Create a new {@code PluginChangedEvent}.operator is unknown.
     *
     * @param source   Current rule state
     * @param before   Before the change rule state
     * @param type     event type
     * @param operator operator
     */
    public RuleChangedEvent(final RuleDO source, final RuleDO before, final EventTypeEnum type, final String operator) {
        super(source, before, type, operator);
    }
    
    @Override
    public String buildContext() {
        final RuleDO after = (RuleDO) getAfter();
        if (Objects.isNull(getBefore())) {
            return String.format("the rule [%s] is %s", after.getName(), StringUtils.lowerCase(getType().getType().toString()));
        }
        return String.format("the rule [%s] is %s : %s", after.getName(), StringUtils.lowerCase(getType().getType().toString()), contrast());
        
    }
    
    private String contrast() {
        final RuleDO before = (RuleDO) getBefore();
        Objects.requireNonNull(before);
        final RuleDO after = (RuleDO) getAfter();
        Objects.requireNonNull(after);
        if (Objects.equals(before, after)) {
            return "it no change";
        }
        final StringBuilder builder = new StringBuilder();
        if (!Objects.equals(before.getName(), after.getName())) {
            builder.append(String.format("name[%s => %s] ", before.getName(), after.getName()));
        }
        if (!Objects.equals(before.getHandle(), after.getHandle())) {
            builder.append(String.format("handle[%s => %s] ", before.getHandle(), after.getHandle()));
        }
        if (!Objects.equals(before.getMatchMode(), after.getMatchMode())) {
            builder.append(String.format("match model[%s => %s] ", before.getMatchMode(), after.getMatchMode()));
        }
        if (!Objects.equals(before.getEnabled(), after.getEnabled())) {
            builder.append(String.format("enable[%s => %s] ", before.getEnabled(), after.getEnabled()));
        }
        if (!Objects.equals(before.getSort(), after.getSort())) {
            builder.append(String.format("sort[%s => %s] ", before.getSort(), after.getSort()));
        }
        if (!Objects.equals(before.getLoged(), after.getLoged())) {
            builder.append(String.format("loged[%s => %s] ", before.getLoged(), after.getLoged()));
        }
        return builder.toString();
    }
    
    @Override
    public String eventName() {
        return "selector";
    }
}
