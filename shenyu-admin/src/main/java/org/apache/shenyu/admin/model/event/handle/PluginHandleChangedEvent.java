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

package org.apache.shenyu.admin.model.event.handle;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.PluginHandleDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;

import java.util.Objects;

/**
 * PluginHandleChangedEvent.
 */
public class PluginHandleChangedEvent extends AdminDataModelChangedEvent {
    
    
    /**
     * Create a new {@code PluginChangedEvent}.operator is unknown.
     *
     * @param source   Current plugin state
     * @param before   Before the change plugiin state
     * @param type     event type
     * @param operator operator
     */
    public PluginHandleChangedEvent(final PluginHandleDO source, final PluginHandleDO before, final EventTypeEnum type, final String operator) {
        super(source, before, type, operator);
    }
    
    @Override
    public String buildContext() {
        final PluginHandleDO after = (PluginHandleDO) getAfter();
        if (Objects.isNull(getBefore())) {
            return String.format("the plugin-handle [%s] is %s", after.getField(), StringUtils.lowerCase(getType().getType().toString()));
        }
        return String.format("the plugin-handle [%s] is %s : %s", after.getField(), StringUtils.lowerCase(getType().getType().toString()), contrast());
    }
    
    private String contrast() {
        final PluginHandleDO before = (PluginHandleDO) getBefore();
        Objects.requireNonNull(before);
        final PluginHandleDO after = (PluginHandleDO) getAfter();
        Objects.requireNonNull(after);
        if (Objects.equals(before, after)) {
            return "it no change";
        }
        final StringBuilder builder = new StringBuilder();
        if (!Objects.equals(before.getField(), after.getField())) {
            builder.append(String.format("field[%s => %s] ", before.getField(), after.getField()));
        }
        if (!Objects.equals(before.getLabel(), after.getLabel())) {
            builder.append(String.format("label[%s => %s] ", before.getLabel(), after.getLabel()));
        }
        if (!Objects.equals(before.getType(), after.getType())) {
            builder.append(String.format("type[%s => %s] ", before.getType(), after.getType()));
        }
        if (!Objects.equals(before.getDataType(), after.getDataType())) {
            builder.append(String.format("dataType[%s => %s] ", before.getDateCreated(), after.getDataType()));
        }
        if (!Objects.equals(before.getSort(), after.getSort())) {
            builder.append(String.format("sort[%s => %s] ", before.getSort(), after.getSort()));
        }
        return builder.toString();
    }
    
    @Override
    public String eventName() {
        return "plugin-handle";
    }
}
