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

package org.apache.shenyu.plugin.sofa.handler;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * SofaMetaDataHandlerTest.
 */
public class SofaMetaDataHandlerTest {

    private SofaMetaDataHandler sofaMetaDataHandler;

    @BeforeEach
    public void setUp() {
        sofaMetaDataHandler = new SofaMetaDataHandler();
    }

    @Test
    public void testPluginNamed() {
        assertEquals(sofaMetaDataHandler.rpcType(), RpcTypeEnum.SOFA.getName());
    }

    @Test
    public void removeTest() {
        assertDoesNotThrow(() -> sofaMetaDataHandler.remove(MetaData.builder().path("path").build()));
    }

    @Test
    public void handleTest() {
        final MetaData metaData = MetaData.builder().path("path").build();
        metaData.setServiceName("serviceName");
        assertDoesNotThrow(() -> sofaMetaDataHandler.handle(metaData));
    }
}
