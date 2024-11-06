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

package org.apache.shenyu.admin.model.event.metadata;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.BatchChangedEvent;
import org.apache.shenyu.common.constant.Constants;

import java.util.Collection;
import java.util.stream.Collectors;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * BatchMetaDataChangedEvent.
 */
public class BatchMetaDataChangedEvent extends BatchChangedEvent {


    /**
     * Create a new {@code BatchMetaDataChangedEvent}.operator is unknown.
     *
     * @param source   Current metadata state
     * @param before   Before the change metadata state
     * @param type     event type
     * @param operator operator
     */
    public BatchMetaDataChangedEvent(final Collection<MetaDataDO> source, final Collection<MetaDataDO> before, final EventTypeEnum type, final String operator) {
        super(source, before, type, operator);
    }

    @Override
    public String buildContext() {
        final String metadata = ((Collection<?>) getSource())
                .stream()
                .map(s -> ((MetaDataDO) s).getAppName())
                .collect(Collectors.joining(","));
        final String namespaceId = ((Collection<?>) getSource())
                .stream()
                .filter(s -> StringUtils.isNotEmpty(((MetaDataDO) s).getNamespaceId()))
                .map(s -> ((MetaDataDO) s).getNamespaceId())
                .findAny().orElse(SYS_DEFAULT_NAMESPACE_ID);
        return String.format("the namespace [%s] meta data [%s] is %s", namespaceId, metadata, StringUtils.lowerCase(getType().getType().toString()));
    }

    @Override
    public String eventName() {
        return Constants.EVENT_NAME_META_DATA;
    }
}
