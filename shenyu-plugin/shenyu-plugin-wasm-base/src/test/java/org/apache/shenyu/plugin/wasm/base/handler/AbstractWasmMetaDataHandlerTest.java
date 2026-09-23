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

import com.dylibso.chicory.runtime.Memory;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The Test Case For AbstractWasmMetaDataHandler.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class AbstractWasmMetaDataHandlerTest {

    private MetaData metaData;
    
    @BeforeEach
    public void setUp() {
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("dubbo");
        metaData.setPath("/dubbo/findAll");
        metaData.setServiceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName("findAll");
        metaData.setRpcType(RpcTypeEnum.DUBBO.getName());
        metaData.setRpcExt("{\"group\":\"Group\",\"version\":\"2.6.5\",\"url\":\"http://192.168.55.113/dubbo\",\"cluster\":\"failover\"}");
        metaData.setParameterTypes("parameterTypes");
    }

    /** Go WASM test. */
    @Test
    public void goHandleMetaDataTest() {
        final TestGoWasmMetaDataHandler goHandler = new TestGoWasmMetaDataHandler("go result");
        goHandler.handle(metaData);
        goHandler.remove(metaData);
        goHandler.refresh();
    }

    /** Rust WASM test. */
    @Test
    public void rustHandleMetaDataTest() {
        final TestWasmMetaDataHandler rustHandler = new TestWasmMetaDataHandler("rust result");
        rustHandler.handle(metaData);
        rustHandler.remove(metaData);
        rustHandler.refresh();
    }

    abstract static class WasmTestHandler extends AbstractWasmMetaDataHandler {
        private static final java.util.concurrent.atomic.AtomicLong ID =
                new java.util.concurrent.atomic.AtomicLong();

        private final String expectedResult;

        WasmTestHandler(final String expectedResult) {
            this.expectedResult = expectedResult;
        }

        @Override
        protected long onGetArgs(final long argId, final long addr, final int len) {
            String config = "hello from java " + argId;
            assertTrue(config.startsWith("hello from java "));
            Memory memory = super.getMemory();
            byte[] data = config.getBytes(StandardCharsets.UTF_8);
            int writeLen = Math.min(len, data.length);
            memory.write((int) addr, data, 0, writeLen);
            return writeLen;
        }

        @Override
        protected long onPutResult(final long argId, final long addr, final int len) {
            Memory memory = super.getMemory();
            byte[] bytes = memory.readBytes((int) addr, len);
            String result = new String(bytes, StandardCharsets.UTF_8);
            assertEquals(expectedResult, result);
            return 0;
        }

        @Override
        public String rpcType() {
            return "wasm";
        }

        @Override
        protected Long getArgumentId(final MetaData metaData) {
            return ID.incrementAndGet();
        }
    }

    /** Loads $TestGoWasmMetaDataHandler.wasm. */
    static class TestGoWasmMetaDataHandler extends WasmTestHandler {
        TestGoWasmMetaDataHandler(final String expected) {
            super(expected);
        }
    }

    /** Loads $TestWasmMetaDataHandler.wasm. */
    static class TestWasmMetaDataHandler extends WasmTestHandler {
        TestWasmMetaDataHandler(final String expected) {
            super(expected);
        }
    }
}
