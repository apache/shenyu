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

import java.util.Arrays;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test case for {@link BatchMetaDataDeletedEvent}.
 */
public class BatchMetaDataDeletedEventTest {

    private MetaDataDO one;

    private MetaDataDO two;

    @BeforeEach
    public void setUp() {
        one = MetaDataDO.builder()
                .id("1")
                .appName("testAppNameOne")
                .path("/testPathOne")
                .pathDesc("testPathDescOne")
                .rpcType("http")
                .serviceName("org.apache.shenyu.examples.http.controller.TestOneController")
                .methodName("post")
                .parameterTypes("java.lang.String")
                .enabled(true)
                .build();

        two = MetaDataDO.builder()
                .id("2")
                .appName("testAppNameTwo")
                .path("/testPathTwo")
                .pathDesc("testPathDescTwo")
                .rpcType("http")
                .serviceName("org.apache.shenyu.examples.http.controller.TestTwoController")
                .methodName("post")
                .parameterTypes("java.lang.String")
                .enabled(true)
                .build();
    }

    @Test
    public void batchMetaDataDeletedContextTest() {
        BatchMetaDataDeletedEvent batchMetaDataChangedEvent =
                new BatchMetaDataDeletedEvent(Arrays.asList(one, two), "test-operator");

        String context = String.format("the meta data [%s] is %s",
                "testAppNameOne,testAppNameTwo", StringUtils.lowerCase(EventTypeEnum.META_DATA_DELETE.getType().toString()));

        assertEquals(context, batchMetaDataChangedEvent.buildContext());
    }
}
