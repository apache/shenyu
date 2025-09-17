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

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;


/**
 * test case for {@link MetaDataCreatedEvent}.
 */
public class MetaDataCreatedEventTest {

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
                .namespaceId(SYS_DEFAULT_NAMESPACE_ID)
                .build();
    }

    @Test
    public void metaDataCreateContextTest() {
        MetaDataChangedEvent createdEvent =
                new MetaDataChangedEvent(createDO, null, EventTypeEnum.META_DATA_CREATE, "test-operator");

        String context = String.format("the namespace [%s] metadata [%s %s] is %s", createDO.getNamespaceId(),
                createDO.getAppName(), createDO.getPath(), StringUtils.lowerCase(EventTypeEnum.META_DATA_CREATE.getType().toString()));

        assertEquals(context, createdEvent.buildContext());
    }
}
