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
import org.apache.shenyu.admin.model.entity.BaseDO;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.BatchChangedEvent;
import org.apache.shenyu.admin.utils.ListUtil;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * BatchResourceDeletedEvent.
 */
public class BatchResourceDeletedEvent extends BatchChangedEvent {
    
    private final List<String> deletedIds;
    
    
    /**
     * Create a new {@code BatchChangedEvent}.operator is unknown.
     *
     * @param source   Current plugin state
     * @param operator operator
     */
    public BatchResourceDeletedEvent(final Collection<ResourceDO> source, final String operator) {
        super(source, null, EventTypeEnum.RESOURCE_DELETE, operator);
        this.deletedIds = ListUtil.map(source, BaseDO::getId);
    }
    
    @Override
    public String buildContext() {
        final String selector = ((Collection<?>) getSource())
                .stream()
                .map(s -> ((ResourceDO) s).getTitle())
                .collect(Collectors.joining(","));
        return String.format("the resource[%s] is %s", selector, StringUtils.lowerCase(getType().getType().toString()));
    }
    
    /**
     * get resource.
     *
     * @return list
     */
    public List<ResourceDO> getResource() {
        return ((Collection<?>) getSource())
                .stream()
                .map(ResourceDO.class::cast)
                .collect(Collectors.toList());
    }
    
    /**
     * get deleted ids.
     *
     * @return ids.
     */
    public List<String> getDeletedIds() {
        return deletedIds;
    }
}
