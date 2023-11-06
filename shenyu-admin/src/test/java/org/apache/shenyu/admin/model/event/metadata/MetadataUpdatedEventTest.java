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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test case for {@link MetadataUpdatedEvent}.
 */
public class MetadataUpdatedEventTest {
    private MetaDataDO updateBeforeDO;

    private MetaDataDO updateAfterDO;

    @BeforeEach
    public void setUp() {
        updateBeforeDO = MetaDataDO.builder()
                .id("1")
                .appName("testUpdateBefore")
                .path("/testUpdateBefore")
                .pathDesc("testUpdateBefore")
                .rpcType("http")
                .serviceName("org.apache.shenyu.examples.http.controller.TestBeforeController")
                .methodName("before")
                .parameterTypes("java.lang.String")
                .enabled(true)
                .rpcExt("rpcExtBefore")
                .build();
        updateAfterDO = MetaDataDO.builder()
                .id("1")
                .appName("testUpdateAfter")
                .path("/testUpdateAfter")
                .pathDesc("testUpdateAfter")
                .rpcType("dubbo")
                .serviceName("org.apache.shenyu.examples.http.controller.TestAfterController")
                .methodName("after")
                .parameterTypes("java.lang.Integer")
                .enabled(false)
                .rpcExt("rpcExtAfter")
                .build();
    }

    @Test
    public void metaDataUpdateContextTest() {
        String eventTypeStr = StringUtils.lowerCase(EventTypeEnum.META_DATA_UPDATE.getType().toString());

        MetaDataChangedEvent updateNothingEvent =
                new MetaDataChangedEvent(updateBeforeDO, updateBeforeDO, EventTypeEnum.META_DATA_UPDATE, "test-operator");
        String baseContext = String.format("the metadata [%s %s] is %s : %s",
                updateBeforeDO.getAppName(), updateBeforeDO.getPath(), eventTypeStr, "it no change");
        assertEquals(baseContext, updateNothingEvent.buildContext());

        final StringBuilder contrast = new StringBuilder();
        contrast.append(String.format("appName[%s => %s] ", updateBeforeDO.getAppName(), updateAfterDO.getAppName()));
        contrast.append(String.format("path[%s => %s] ", updateBeforeDO.getPath(), updateAfterDO.getPath()));
        contrast.append(String.format("path desc[%s => %s] ", updateBeforeDO.getPathDesc(), updateAfterDO.getPathDesc()));
        contrast.append(String.format("service[%s => %s] ", updateBeforeDO.getServiceName(), updateAfterDO.getServiceName()));
        contrast.append(String.format("method[%s => %s] ", updateBeforeDO.getMethodName(), updateAfterDO.getMethodName()));
        contrast.append(String.format("parameter type[%s => %s] ", updateBeforeDO.getParameterTypes(), updateAfterDO.getParameterTypes()));
        contrast.append(String.format("enable[%s => %s] ", updateBeforeDO.getEnabled(), updateAfterDO.getEnabled()));
        contrast.append(String.format("rpc type[%s => %s] ", updateBeforeDO.getRpcType(), updateAfterDO.getRpcType()));
        contrast.append(String.format("rpc ext[%s => %s] ", updateBeforeDO.getRpcExt(), updateAfterDO.getRpcExt()));

        String context = String.format("the metadata [%s %s] is %s : %s",
                updateAfterDO.getAppName(), updateAfterDO.getPath(), eventTypeStr, contrast);

        MetaDataChangedEvent updateEvent =
                new MetaDataChangedEvent(updateAfterDO, updateBeforeDO, EventTypeEnum.META_DATA_UPDATE, "test-operator");
        assertEquals(context, updateEvent.buildContext());
    }

}
