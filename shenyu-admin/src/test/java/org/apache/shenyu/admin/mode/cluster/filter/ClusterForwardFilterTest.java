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

package org.apache.shenyu.admin.mode.cluster.filter;

import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockFilterChain;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;
import org.yaml.snakeyaml.Yaml;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ClusterForwardFilter}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ClusterForwardFilterTest {

    private static final String MASTER_HOST = "10.0.0.9";

    private static final String MASTER_PORT = "9095";

    @Mock
    private RestTemplate restTemplate;

    @Mock
    private ClusterSelectMasterService clusterSelectMasterService;

    private ClusterForwardFilter clusterForwardFilter;

    private ClusterProperties clusterProperties;

    @BeforeEach
    public void setUp() {
        clusterForwardFilter = new ClusterForwardFilter(restTemplate);
        clusterProperties = new ClusterProperties();
        ReflectionTestUtils.setField(clusterForwardFilter, "clusterSelectMasterService", clusterSelectMasterService);
        ReflectionTestUtils.setField(clusterForwardFilter, "clusterProperties", clusterProperties);

        final ClusterMasterDTO master = new ClusterMasterDTO();
        master.setMasterHost(MASTER_HOST);
        master.setMasterPort(MASTER_PORT);
        master.setContextPath("");
        when(clusterSelectMasterService.getMaster()).thenReturn(master);
    }

    @Test
    public void testMasterHandlesRequestsLocally() throws Exception {
        when(clusterSelectMasterService.isMaster()).thenReturn(true);

        final MockHttpServletRequest request = beatRequest();
        final MockFilterChain filterChain = new MockFilterChain();
        clusterForwardFilter.doFilter(request, new MockHttpServletResponse(), filterChain);

        assertNotNull(filterChain.getRequest(), "master must handle the request locally");
        verify(restTemplate, never()).exchange(anyString(), any(HttpMethod.class), any(HttpEntity.class), eq(byte[].class));
    }

    @Test
    public void testInstanceBeatForwardedToMaster() throws Exception {
        when(clusterSelectMasterService.isMaster()).thenReturn(false);
        clusterProperties.setForwardList(Collections.singletonList("/instance/beat"));

        final MockHttpServletRequest request = beatRequest();
        final MockFilterChain filterChain = new MockFilterChain();
        final MockHttpServletResponse response = new MockHttpServletResponse();
        when(restTemplate.exchange(anyString(), any(HttpMethod.class), any(HttpEntity.class), eq(byte[].class)))
                .thenReturn(new ResponseEntity<>("success".getBytes(StandardCharsets.UTF_8), HttpStatus.OK));

        clusterForwardFilter.doFilter(request, response, filterChain);

        final ArgumentCaptor<String> urlCaptor = ArgumentCaptor.forClass(String.class);
        final ArgumentCaptor<HttpEntity<byte[]>> entityCaptor = ArgumentCaptor.forClass(HttpEntity.class);
        verify(restTemplate).exchange(urlCaptor.capture(), eq(HttpMethod.POST), entityCaptor.capture(), eq(byte[].class));
        assertTrue(urlCaptor.getValue().contains(MASTER_HOST + ":" + MASTER_PORT), urlCaptor.getValue());
        assertTrue(urlCaptor.getValue().contains("/instance/beat"), urlCaptor.getValue());
        assertEquals("{\"instanceIp\":\"127.0.0.1\"}", new String(entityCaptor.getValue().getBody(), StandardCharsets.UTF_8));
        assertEquals("application/json", entityCaptor.getValue().getHeaders().getFirst("Content-Type"));
        assertNull(filterChain.getRequest(), "forwarded requests must not run the local chain");
    }

    @Test
    public void testUnlistedPathHandledLocally() throws Exception {
        when(clusterSelectMasterService.isMaster()).thenReturn(false);
        clusterProperties.setForwardList(Collections.singletonList("/selector/**"));

        final MockHttpServletRequest request = beatRequest();
        final MockFilterChain filterChain = new MockFilterChain();
        clusterForwardFilter.doFilter(request, new MockHttpServletResponse(), filterChain);

        assertNotNull(filterChain.getRequest(), "paths outside the forward list must be handled locally");
        verify(restTemplate, never()).exchange(anyString(), any(HttpMethod.class), any(HttpEntity.class), eq(byte[].class));
    }

    @Test
    public void testApplicationYmlForwardsInstanceBeat() throws Exception {
        final Yaml yaml = new Yaml();
        // the main application.yml (the test classpath one is a test fixture without the cluster section)
        try (InputStream in = java.nio.file.Files.newInputStream(java.nio.file.Paths.get("src", "main", "resources", "application.yml"))) {
            final Map<String, Object> root = yaml.load(in);
            final Map<String, Object> shenyu = castMap(root.get("shenyu"));
            final Map<String, Object> cluster = castMap(shenyu.get("cluster"));
            final List<String> forwardList = castList(cluster.get("forward-list"));
            assertTrue(forwardList.contains("/instance/beat"),
                    "instance heartbeats must be forwarded to the master admin");
        }
    }

    private MockHttpServletRequest beatRequest() {
        final MockHttpServletRequest request = new MockHttpServletRequest();
        request.setMethod("POST");
        request.setRequestURI("/instance/beat");
        request.setContextPath("");
        request.setContentType("application/json");
        request.setContent("{\"instanceIp\":\"127.0.0.1\"}".getBytes(StandardCharsets.UTF_8));
        return request;
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> castMap(final Object value) {
        return (Map<String, Object>) value;
    }

    @SuppressWarnings("unchecked")
    private List<String> castList(final Object value) {
        return (List<String>) value;
    }
}
