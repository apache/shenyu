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

package org.apache.shenyu.admin.model.event.dict;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;

import java.util.Objects;

/**
 * DictChangedEvent.
 */
public class DictChangedEvent extends AdminDataModelChangedEvent {
    
    
    /**
     * Create a new {@code DictChangedEvent}.operator is unknown.
     *
     * @param source   Current dict state
     * @param before   Before the change dict state
     * @param type     event type
     * @param operator operator
     */
    public DictChangedEvent(final ShenyuDictDO source, final ShenyuDictDO before, final EventTypeEnum type, final String operator) {
        super(source, before, type, operator);
    }
    
    @Override
    public String buildContext() {
        final ShenyuDictDO after = (ShenyuDictDO) getAfter();
        if (Objects.isNull(getBefore())) {
            return String.format("the dict [%s] is %s", after.getDictName(), StringUtils.lowerCase(getType().getType().toString()));
        }
        return String.format("the dict [%s] is %s : %s", after.getDictName(), StringUtils.lowerCase(getType().getType().toString()), contrast());
        
    }
    
    private String contrast() {
        final ShenyuDictDO before = (ShenyuDictDO) getBefore();
        Objects.requireNonNull(before);
        final ShenyuDictDO after = (ShenyuDictDO) getAfter();
        Objects.requireNonNull(after);
        if (Objects.equals(before, after)) {
            return "it no change";
        }
        final StringBuilder builder = new StringBuilder();
        if (!Objects.equals(before.getDictName(), after.getDictName())) {
            builder.append(String.format("name[%s => %s] ", before.getDictName(), after.getDictName()));
        }
        if (!Objects.equals(before.getDictValue(), after.getDictValue())) {
            builder.append(String.format("value[%s => %s] ", before.getDictValue(), after.getDictValue()));
        }
        if (!Objects.equals(before.getDesc(), after.getDesc())) {
            builder.append(String.format("desc[%s => %s] ", before.getDesc(), after.getDesc()));
        }
        if (!Objects.equals(before.getType(), after.getType())) {
            builder.append(String.format("type[%s => %s] ", before.getType(), after.getType()));
        }
        if (!Objects.equals(before.getEnabled(), after.getEnabled())) {
            builder.append(String.format("enable[%s => %s] ", before.getEnabled(), after.getEnabled()));
        }
        if (!Objects.equals(before.getSort(), after.getSort())) {
            builder.append(String.format("sort[%s => %s] ", before.getSort(), after.getSort()));
        }
        return builder.toString();
    }
    
    @Override
    public String eventName() {
        return "dict";
    }
}
