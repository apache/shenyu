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

package org.apache.shenyu.admin.model.event.selector;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;
import org.apache.shenyu.common.constant.Constants;

import java.util.Objects;

/**
 * SelectorChangedEvent.
 */
public class SelectorChangedEvent extends AdminDataModelChangedEvent {
    
    
    /**
     * Create a new {@code PluginChangedEvent}.operator is unknown.
     *
     * @param source   Current plugin state
     * @param before   Before the change plugiin state
     * @param type     event type
     * @param operator operator
     */
    public SelectorChangedEvent(final SelectorDO source, final SelectorDO before, final EventTypeEnum type, final String operator) {
        super(source, before, type, operator);
    }
    
    @Override
    public String buildContext() {
        final SelectorDO after = (SelectorDO) getAfter();
        if (Objects.isNull(getBefore())) {
            return String.format("the namespace [%s] selector [%s] is %s", after.getNamespaceId(), after.getSelectorName(), StringUtils.lowerCase(getType().getType().toString()));
        }
        return String.format("the namespace [%s] selector [%s] is %s : %s", after.getNamespaceId(), after.getSelectorName(), StringUtils.lowerCase(getType().getType().toString()), contrast());
        
    }
    
    private String contrast() {
        final SelectorDO before = (SelectorDO) getBefore();
        Objects.requireNonNull(before);
        final SelectorDO after = (SelectorDO) getAfter();
        Objects.requireNonNull(after);
        if (Objects.equals(before, after)) {
            return "it no change";
        }
        final StringBuilder builder = new StringBuilder();
        if (!Objects.equals(before.getSelectorName(), after.getSelectorName())) {
            builder.append(String.format("name[%s => %s] ", before.getSelectorName(), after.getSelectorName()));
        }
        if (!Objects.equals(before.getHandle(), after.getHandle())) {
            builder.append(String.format("handle[%s => %s] ", before.getHandle(), after.getHandle()));
        }
        if (!Objects.equals(before.getSelectorType(), after.getSelectorType())) {
            builder.append(String.format("type[%s => %s] ", before.getSelectorType(), after.getSelectorType()));
        }
        if (!Objects.equals(before.getEnabled(), after.getEnabled())) {
            builder.append(String.format("enable[%s => %s] ", before.getEnabled(), after.getEnabled()));
        }
        if (!Objects.equals(before.getSortCode(), after.getSortCode())) {
            builder.append(String.format("sort[%s => %s] ", before.getSortCode(), after.getSortCode()));
        }
        if (!Objects.equals(before.getLoged(), after.getLoged())) {
            builder.append(String.format("loged[%s => %s] ", before.getLoged(), after.getLoged()));
        }
        if (!Objects.equals(before.getNamespaceId(), after.getNamespaceId())) {
            builder.append(String.format("namespaceId[%s => %s] ", before.getNamespaceId(), after.getNamespaceId()));
        }
        return builder.toString();
    }
    
    @Override
    public String eventName() {
        return Constants.EVENT_NAME_SELECTOR;
    }
}
