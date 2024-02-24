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

package org.apache.shenyu.plugin.wasm.base.handler;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AbstractWasmMetaDataHandlerTest {

    private MetaData metaData;

    private TestWasmMetaDataHandler testWasmMetaDataHandler;

    private MetaDataHandler metaDataHandler;

    @BeforeEach
    public void setUp() {
        this.metaData = mock(MetaData.class);
        this.testWasmMetaDataHandler = new TestWasmMetaDataHandler();
        this.metaDataHandler = testWasmMetaDataHandler;
        when(metaData.getId()).thenReturn("SHENYU");
    }

    /**
     * The handle test.
     */
    @Test
    public void handleTest() {
        metaDataHandler.handle(metaData);
        testWasmMetaDataHandler.handle(metaData);
        assertEquals(testWasmMetaDataHandler.getMetaDataMap().get(metaData.getId()).getId(), metaData.getId());
    }

    /**
     * The remove metaData test.
     */
    @Test
    public void removeTest() {
        metaDataHandler.remove(metaData);
        testWasmMetaDataHandler.handle(metaData);
        testWasmMetaDataHandler.remove(metaData);
        assertNull(testWasmMetaDataHandler.getMetaDataMap().get(metaData.getId()));
    }

    /**
     * The refresh test.
     */

    @Test
        public void refreshTest() {
        MetaDataHandler metaDataHandler = mock(MetaDataHandler.class);
        metaDataHandler.refresh();
        testWasmMetaDataHandler.refresh();
        Mockito.verify(metaDataHandler, Mockito.times(1)).refresh();
    }
        /**
         * The rpc type test.
         */

    @Test
    public void rpcTypeTest() {
        assertEquals("SHENYU_TEST", metaDataHandler.rpcType());
        assertEquals("SHENYU_TEST", testWasmMetaDataHandler.rpcType());
    }

    static class TestWasmMetaDataHandler implements MetaDataHandler {

        private Map<String, MetaData> metaDataMap = new HashMap<>();

        public Map<String, MetaData> getMetaDataMap() {
            return metaDataMap;
        }

        public void setMetaDataMap(final Map<String, MetaData> metaDataMap) {
            this.metaDataMap = metaDataMap;
        }

        @Override
        public void handle(final MetaData metaData) {
            metaDataMap.put(metaData.getId(), metaData);
        }

        @Override
        public void remove(final MetaData metaData) {
            metaDataMap.remove(metaData.getId());
        }

        @Override
        public String rpcType() {
            return "SHENYU_TEST";
        }
    }
}

