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

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for FallbackShenyuClientRegisterService.
 */
class FallbackShenyuClientRegisterServiceTest {

    @Test
    public void testRegisterURI() {
        MockFallbackShenyuClientRegisterService mockFallbackShenyuClientRegisterService = new MockFallbackShenyuClientRegisterService();
        assertEquals("doRegisterURI", mockFallbackShenyuClientRegisterService.registerURI("Selector_Name", new ArrayList<>()));

        MockFallbackShenyuClientRegisterServiceException mockFallbackShenyuClientRegisterServiceException = new MockFallbackShenyuClientRegisterServiceException();
        assertEquals(StringUtils.EMPTY, mockFallbackShenyuClientRegisterServiceException.registerURI("Selector_Name", new ArrayList<>()));
    }

    static class MockFallbackShenyuClientRegisterService extends FallbackShenyuClientRegisterService {

        @Override
        String doRegisterURI(final String selectorName, final List<URIRegisterDTO> uriList) {
            return "doRegisterURI";
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
    }

    static class MockFallbackShenyuClientRegisterServiceException extends FallbackShenyuClientRegisterService {

        @Override
        String doRegisterURI(final String selectorName, final List<URIRegisterDTO> uriList) {
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
    }
}
