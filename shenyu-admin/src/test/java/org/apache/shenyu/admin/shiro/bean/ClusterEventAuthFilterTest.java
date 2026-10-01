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

package org.apache.shenyu.admin.shiro.bean;

import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercise the node authentication filter, not a direct controller call.
 */
public final class ClusterEventAuthFilterTest {

    @Test
    public void testOrdinaryUserCannotPublishButNodeCan() throws Exception {
        ClusterProperties properties = new ClusterProperties();
        properties.setEnabled(true);
        properties.setEventSecret("dedicated-node-secret");
        ClusterEventAuthFilter filter = new ClusterEventAuthFilter(properties);
        filter.processPathConfig("/cluster/data-change-event", null);
        MockHttpServletRequest request = new MockHttpServletRequest("POST", "/cluster/data-change-event");
        request.setServletPath("/cluster/data-change-event");
        request.addHeader("X-Access-Token", "ordinary-user-token");
        MockHttpServletResponse response = new MockHttpServletResponse();
        AtomicBoolean reachedController = new AtomicBoolean();
        filter.doFilter(request, response, (req, res) -> reachedController.set(true));
        assertEquals(403, response.getStatus());
        assertFalse(reachedController.get());
        request.addHeader(ClusterEventAuthFilter.HEADER, "wrong-secret");
        filter.doFilter(request, new MockHttpServletResponse(), (req, res) -> reachedController.set(true));
        assertFalse(reachedController.get());
        request.removeHeader(ClusterEventAuthFilter.HEADER);
        request.addHeader(ClusterEventAuthFilter.HEADER, "dedicated-node-secret");
        filter.doFilter(request, new MockHttpServletResponse(), (req, res) -> reachedController.set(true));
        assertTrue(reachedController.get());
        reachedController.set(false);
        properties.setEventSecret("");
        filter.doFilter(request, new MockHttpServletResponse(), (req, res) -> reachedController.set(true));
        assertFalse(reachedController.get());
    }
}
