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
import org.apache.shenyu.admin.model.event.BatchChangedEvent;

import java.util.Collection;
import java.util.stream.Collectors;

/**
 * BatchDictChangedEvent.
 */
public class BatchDictChangedEvent extends BatchChangedEvent {
    
    
    /**
     * Create a new {@code BatchDictChangedEvent}.operator is unknown.
     *
     * @param source   Current dict state
     * @param before   Before the change dict state
     * @param type     event type
     * @param operator operator
     */
    public BatchDictChangedEvent(final Collection<ShenyuDictDO> source, final Collection<ShenyuDictDO> before, final EventTypeEnum type, final String operator) {
        super(source, before, type, operator);
    }
    
    @Override
    public String buildContext() {
        final String plugins = ((Collection<?>) getSource())
                .stream()
                .map(s -> ((ShenyuDictDO) s).getDictName())
                .collect(Collectors.joining(","));
        return String.format("the shenyu dict[%s] is %s", plugins, StringUtils.lowerCase(getType().getType().toString()));
    }
    
    @Override
    public String eventName() {
        return "dict";
    }
}
