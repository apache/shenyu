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

package org.apache.shenyu.admin.service.register;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Map;
import java.util.Collections;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test cases for FallbackShenyuClientRegisterService.
 */
class FallbackShenyuClientRegisterServiceTest {

    @Test
    public void testRegisterURI() {

        MockFallbackShenyuClientRegisterService mockFallbackShenyuClientRegisterService = new MockFallbackShenyuClientRegisterService();
        assertEquals("doRegisterURI", mockFallbackShenyuClientRegisterService.registerURI("Selector_Name", new ArrayList<>(), SYS_DEFAULT_NAMESPACE_ID));

        MockFallbackShenyuClientRegisterServiceException mockFallbackShenyuClientRegisterServiceException = new MockFallbackShenyuClientRegisterServiceException();
        assertEquals(StringUtils.EMPTY, mockFallbackShenyuClientRegisterServiceException.registerURI("Selector_Name", new ArrayList<>(), SYS_DEFAULT_NAMESPACE_ID));
    }


    @Test
    public void fallbackRetryIsNamespaced() throws Exception {
        final NamespacedFailService service = new NamespacedFailService();
        // namespace A fails and queues a pending retry
        service.registerURI("orders", new ArrayList<>(), "ns-a");
        // namespace B registers the same selector name successfully
        service.registerURI("orders", new ArrayList<>(), "ns-b");

        @SuppressWarnings("unchecked")
        final Map<String, Object> falls = (Map<String, Object>) ReflectionTestUtils.getField(service, "fallsRegisters");
        assertTrue(falls.keySet().stream().anyMatch(key -> key.endsWith(":ns-a")),
                "namespace A's pending retry must survive namespace B's registration of the same selector name");
        assertTrue(falls.keySet().stream().noneMatch(key -> key.endsWith(":ns-b")),
                "namespace B's successful registration must not leave a pending retry");
    }

    @Test
    public void testHeartbeat() {

        MockFallbackShenyuClientRegisterService mockService = new MockFallbackShenyuClientRegisterService();
        List<URIRegisterDTO> uriList = Collections.singletonList(new URIRegisterDTO());
        String result = mockService.heartbeat("Selector_Name", uriList, SYS_DEFAULT_NAMESPACE_ID);

        assertEquals("doHeartbeat", result);

        MockFallbackShenyuClientRegisterServiceException mockExceptionService = new MockFallbackShenyuClientRegisterServiceException();

        assertThrows(ShenyuException.class, () -> mockExceptionService.heartbeat("Selector_Name", uriList, SYS_DEFAULT_NAMESPACE_ID));
    }

    @Test
    public void testReadinessDelegation() {
        MockFallbackShenyuClientRegisterService mockService = new MockFallbackShenyuClientRegisterService();
        List<URIRegisterDTO> uriList = Collections.singletonList(new URIRegisterDTO());

        assertEquals("doNotReady", mockService.notReady("Selector_Name", uriList, SYS_DEFAULT_NAMESPACE_ID));
        assertEquals("doReady", mockService.ready("Selector_Name", uriList, SYS_DEFAULT_NAMESPACE_ID));

        MockFallbackShenyuClientRegisterServiceException mockExceptionService = new MockFallbackShenyuClientRegisterServiceException();
        assertThrows(ShenyuException.class, () -> mockExceptionService.notReady("Selector_Name", uriList, SYS_DEFAULT_NAMESPACE_ID));
        assertThrows(ShenyuException.class, () -> mockExceptionService.ready("Selector_Name", uriList, SYS_DEFAULT_NAMESPACE_ID));
    }

    @Test
    public void testRpcType() {

        MockFallbackShenyuClientRegisterService mockService = new MockFallbackShenyuClientRegisterService();

        assertEquals("grpc", mockService.rpcType());
    }

    @Test
    public void testRegisterMetaDataAndApiDoc() {

        MockFallbackShenyuClientRegisterService mockService = new MockFallbackShenyuClientRegisterService();

        assertEquals(null, mockService.register(new MetaDataRegisterDTO()));
        assertEquals(null, mockService.registerApiDoc(new ApiDocRegisterDTO()));
    }

    @Test
    public void testCheckNamespacePluginRel() {
        MockFallbackShenyuClientRegisterService mockService = new MockFallbackShenyuClientRegisterService();
        // Should not throw any exception
        mockService.checkNamespacePluginRel("namespace", "plugin");
    }


    static class NamespacedFailService extends FallbackShenyuClientRegisterService {

        @Override
        String doRegisterURI(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            if ("ns-a".equals(namespaceId)) {
                throw new RuntimeException("namespace A registration backend down");
            }
            return "ok";
        }

        @Override
        String doHeartbeat(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            return "ok";
        }

        @Override
        String doDraining(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            return "ok";
        }

        @Override
        String doNotReady(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            return "ok";
        }

        @Override
        String doReady(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            return "ok";
        }

        @Override
        public String rpcType() {
            return "grpc";
        }

        @Override
        public String register(final MetaDataRegisterDTO metaDataRegisterDTO) {
            return null;
        }

        @Override
        public String registerApiDoc(final ApiDocRegisterDTO apiDocRegisterDTO) {
            return null;
        }

        @Override
        public void checkNamespacePluginRel(final String namespaceId, final String pluginName) {
        }
    }

    static class MockFallbackShenyuClientRegisterService extends FallbackShenyuClientRegisterService {

        @Override
        String doRegisterURI(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            return "doRegisterURI";
        }
        
        @Override
        String doHeartbeat(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            return "doHeartbeat";
        }

        @Override
        String doDraining(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            return "doDraining";
        }

        @Override
        String doNotReady(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            return "doNotReady";
        }

        @Override
        String doReady(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            return "doReady";
        }
        
        @Override
        public String rpcType() {
            return "grpc";
        }

        @Override
        public String register(final MetaDataRegisterDTO metaDataRegisterDTO) {
            return null;
        }

        @Override
        public String registerApiDoc(final ApiDocRegisterDTO apiDocRegisterDTO) {
            return null;
        }

        @Override
        public void checkNamespacePluginRel(final String namespaceId, final String pluginName) {

        }
    }

    static class MockFallbackShenyuClientRegisterServiceException extends FallbackShenyuClientRegisterService {

        @Override
        String doRegisterURI(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            throw new ShenyuException("Exception");
        }
        
        @Override
        String doHeartbeat(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            throw new ShenyuException("Exception");
        }

        @Override
        String doDraining(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            throw new ShenyuException("Exception");
        }

        @Override
        String doNotReady(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            throw new ShenyuException("Exception");
        }

        @Override
        String doReady(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
            throw new ShenyuException("Exception");
        }
        
        @Override
        public String rpcType() {
            return "grpc";
        }

        @Override
        public String register(final MetaDataRegisterDTO metaDataRegisterDTO) {
            return null;
        }

        @Override
        public String registerApiDoc(final ApiDocRegisterDTO apiDocRegisterDTO) {
            return null;
        }

        @Override
        public void checkNamespacePluginRel(final String namespaceId, final String pluginName) {

        }
    }
}
