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
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;

import java.util.Objects;

/**
 * MetaDatarChangedEvent.
 */
public class MetaDataChangedEvent extends AdminDataModelChangedEvent {
    
    
    /**
     * Create a new {@code PluginChangedEvent}.operator is unknown.
     *
     * @param source   Current plugin state
     * @param before   Before the change plugiin state
     * @param type     event type
     * @param operator operator
     */
    public MetaDataChangedEvent(final MetaDataDO source, final MetaDataDO before, final EventTypeEnum type, final String operator) {
        super(source, before, type, operator);
    }
    
    @Override
    public String buildContext() {
        final MetaDataDO after = (MetaDataDO) getAfter();
        if (Objects.isNull(getBefore())) {
            return String.format("the metadata [%s %s] is %s", after.getAppName(), after.getPath(), StringUtils.lowerCase(getType().getType().toString()));
        }
        return String.format("the metadata [%s %s] is %s : %s", after.getAppName(), after.getPath(), StringUtils.lowerCase(getType().getType().toString()), contrast());
        
    }
    
    private String contrast() {
        final MetaDataDO before = (MetaDataDO) getBefore();
        Objects.requireNonNull(before);
        final MetaDataDO after = (MetaDataDO) getAfter();
        Objects.requireNonNull(after);
        if (Objects.equals(before, after)) {
            return "it no change";
        }
        final StringBuilder builder = new StringBuilder();
        if (!Objects.equals(before.getAppName(), after.getAppName())) {
            builder.append(String.format("appName[%s => %s] ", before.getAppName(), after.getAppName()));
        }
        if (!Objects.equals(before.getPath(), after.getPath())) {
            builder.append(String.format("path[%s => %s] ", before.getPath(), after.getPath()));
        }
        if (!Objects.equals(before.getPathDesc(), after.getPathDesc())) {
            builder.append(String.format("path desc[%s => %s] ", before.getPathDesc(), after.getPathDesc()));
        }
        if (!Objects.equals(before.getEnabled(), after.getEnabled())) {
            builder.append(String.format("enable[%s => %s] ", before.getEnabled(), after.getEnabled()));
        }
        if (!Objects.equals(before.getServiceName(), after.getServiceName())) {
            builder.append(String.format("service[%s => %s] ", before.getServiceName(), after.getServiceName()));
        }
        if (!Objects.equals(before.getMethodName(), after.getMethodName())) {
            builder.append(String.format("method[%s => %s] ", before.getMethodName(), after.getMethodName()));
        }
        if (!Objects.equals(before.getParameterTypes(), after.getParameterTypes())) {
            builder.append(String.format("parameter type [%s => %s] ", before.getParameterTypes(), after.getParameterTypes()));
        }
        if (!Objects.equals(before.getEnabled(), after.getEnabled())) {
            builder.append(String.format("enable [%s => %s] ", before.getEnabled(), after.getEnabled()));
        }
        if (!Objects.equals(before.getRpcType(), after.getRpcType())) {
            builder.append(String.format("rpc type [%s => %s] ", before.getRpcType(), after.getRpcType()));
        }
        if (!Objects.equals(before.getRpcExt(), after.getRpcExt())) {
            builder.append(String.format("rpc ext [%s => %s] ", before.getRpcExt(), after.getRpcExt()));
        }
        return builder.toString();
    }
    
    @Override
    public String eventName() {
        return "meta data";
    }
}
