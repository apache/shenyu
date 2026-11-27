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

package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.config.properties.RegistrationFencingProperties;
import org.apache.shenyu.admin.config.properties.RegistrationLeaseProperties;
import org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol;
import org.apache.shenyu.register.common.protocol.RegistrationLeaseProtocol;
import org.apache.shenyu.register.common.protocol.RegistrationProtocol;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The lease capability is advertised only when the lease is enabled, alongside the
 * existing capabilities and without touching the v1 version negotiation.
 */
class RegistrationProtocolInterceptorLeaseTest {

    @Test
    void leaseCapabilityIsAbsentWhenDisabled() throws java.io.IOException {
        RegistrationProtocolInterceptor interceptor = new RegistrationProtocolInterceptor(
                new RegistrationFencingProperties(), new RegistrationLeaseProperties());
        MockHttpServletResponse response = new MockHttpServletResponse();

        interceptor.preHandle(new MockHttpServletRequest("POST", "/instance/beat"), response, new Object());

        assertEquals(RegistrationProtocol.V1_CAPABILITIES, response.getHeader(RegistrationProtocol.CAPABILITIES_HEADER));
    }

    @Test
    void leaseCapabilityIsAdvertisedWhenEnabled() throws java.io.IOException {
        RegistrationLeaseProperties leaseProperties = new RegistrationLeaseProperties();
        leaseProperties.setEnabled(true);
        RegistrationFencingProperties fencingProperties = new RegistrationFencingProperties();
        fencingProperties.setEnabled(true);
        RegistrationProtocolInterceptor interceptor = new RegistrationProtocolInterceptor(fencingProperties, leaseProperties);
        MockHttpServletResponse response = new MockHttpServletResponse();

        interceptor.preHandle(new MockHttpServletRequest("POST", "/instance/beat"), response, new Object());

        String capabilities = response.getHeader(RegistrationProtocol.CAPABILITIES_HEADER);
        assertTrue(capabilities.contains(RegistrationLeaseProtocol.CAPABILITY), capabilities);
        assertTrue(capabilities.contains(RegistrationFencingProtocol.CAPABILITY), capabilities);
        assertEquals(RegistrationProtocol.V1, response.getHeader(RegistrationProtocol.VERSION_HEADER));
    }
}
