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

import io.github.kawamuray.wasmtime.Func;
import io.github.kawamuray.wasmtime.Store;
import io.github.kawamuray.wasmtime.WasmFunctions;
import io.github.kawamuray.wasmtime.WasmValType;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;

/**
 * The Test Case For AbstractWasmMetaDataHandler.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class AbstractWasmMetaDataHandlerTest {
    
    private TestWasmMetaDataHandler testWasmMetaDataHandler;
    
    private MetaData metaData;
    
    @BeforeEach
    public void setUp() {
        testWasmMetaDataHandler = new TestWasmMetaDataHandler();
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
    
    @Test
    public void testOnSubscribe() {
        testWasmMetaDataHandler.handle(metaData);
        MetaData metaData = MetaData.builder()
                .id("1332017966661636096")
                .appName("dubbo")
                .path("/dubbo/findAll")
                .serviceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService")
                .methodName("findById")
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .rpcExt("{\"group\":\"Group\",\"version\":\"2.6.5\",\"url\":\"http://192.168.55.113/dubbo\",\"cluster\":\"failover\"}")
                .parameterTypes("parameterTypes").build();
        TestWasmMetaDataHandler testWasmMetaDataHandlerMock = mock(TestWasmMetaDataHandler.class);
        doNothing().when(testWasmMetaDataHandlerMock).handle(metaData);
        testWasmMetaDataHandlerMock.handle(metaData);
        testWasmMetaDataHandler.handle(metaData);
        testWasmMetaDataHandler.remove(metaData);
        testWasmMetaDataHandler.refresh();
    }
    
    static class TestWasmMetaDataHandler extends AbstractWasmMetaDataHandler {
        
        private static final Logger LOG = LoggerFactory.getLogger(TestWasmMetaDataHandler.class);
        
        @Override
        protected Map<String, Func> initWasmCallJavaFunc(final Store<Void> store) {
            Map<String, Func> funcMap = new HashMap<>();
            funcMap.put("get_args", WasmFunctions.wrap(store, WasmValType.I64, WasmValType.I64, WasmValType.I32, WasmValType.I32,
                (argId, addr, len) -> {
                    String config = "hello from java " + argId;
                    LOG.info("java side->{}", config);
                    assertEquals("hello from java 0", config);
                    ByteBuffer buf = super.getBuffer();
                    for (int i = 0; i < len && i < config.length(); i++) {
                        buf.put(addr.intValue() + i, (byte) config.charAt(i));
                    }
                    return Math.min(config.length(), len);
                }));
            funcMap.put("put_result", WasmFunctions.wrap(store, WasmValType.I64, WasmValType.I64, WasmValType.I32, WasmValType.I32,
                (argId, addr, len) -> {
                    ByteBuffer buf = super.getBuffer();
                    byte[] bytes = new byte[len];
                    for (int i = 0; i < len; i++) {
                        bytes[i] = buf.get(addr.intValue() + i);
                    }
                    String result = new String(bytes, StandardCharsets.UTF_8);
                    assertEquals("rust result", result);
                    LOG.info("java side->{}", result);
                    return 0;
                }));
            return funcMap;
        }
        
        @Override
        public String rpcType() {
            return "wasm";
        }
        
        @Override
        protected Long getArgumentId(final MetaData metaData) {
            return 0L;
        }
    }
}
