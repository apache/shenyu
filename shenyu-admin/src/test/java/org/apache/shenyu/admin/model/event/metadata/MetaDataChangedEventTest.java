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
 * test case for {@link MetaDataChangedEvent}.
 */
public class MetaDataChangedEventTest {
    private MetaDataDO createDO;

    @BeforeEach
    public void setUp() {
        createDO = MetaDataDO.builder()
                .id("1")
                .appName("test-app")
                .path("/test")
                .pathDesc("test")
                .rpcType("http")
                .serviceName("org.apache.shenyu.examples.http.controller.TestController")
                .methodName("post")
                .parameterTypes("java.lang.String")
                .rpcExt("test")
                .enabled(true)
                .build();
    }

    @Test
    public void metaDataDeleteContextTest() {
        MetaDataChangedEvent deleteEvent =
                new MetaDataChangedEvent(createDO, null, EventTypeEnum.META_DATA_DELETE, "test-operator");

        String context = String.format("the metadata [%s %s] is %s",
                createDO.getAppName(), createDO.getPath(), StringUtils.lowerCase(EventTypeEnum.META_DATA_DELETE.getType().toString()));

        assertEquals(context, deleteEvent.buildContext());
    }

}
