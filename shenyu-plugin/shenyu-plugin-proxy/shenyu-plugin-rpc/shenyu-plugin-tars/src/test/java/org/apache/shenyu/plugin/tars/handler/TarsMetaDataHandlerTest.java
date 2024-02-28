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

package org.apache.shenyu.plugin.tars.handler;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Test case for {@link org.apache.shenyu.plugin.tars.handler.TarsMetaDataHandler}.
 */
@ExtendWith(MockitoExtension.class)
public class TarsMetaDataHandlerTest {

    private TarsMetaDataHandler tarsMetaDataHandler;

    private MetaData metaData;

    @BeforeEach
    public void setUp() {
        metaData = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path", RpcTypeEnum.TARS.getName(), "serviceName", "method1",
                "parameterTypes", "{\"methodInfo\":[{\"methodName\":\"method1\",\"params\":[{\"left\":\"int\",\"right\":\"param1\"},"
                + "{\"left\":\"java.lang.Integer\",\"right\":\"param2\"}],\"returnType\":\"java.lang.String\"}]}", false);
        tarsMetaDataHandler = new TarsMetaDataHandler();
    }

    @Test
    public void testOnSubscribe() {
        tarsMetaDataHandler.handle(metaData);
        /**
         * test for cache;
         */
        tarsMetaDataHandler.handle(metaData);
    }

    @Test
    public void testUnSubscribe() {
        tarsMetaDataHandler.remove(metaData);
    }
}
