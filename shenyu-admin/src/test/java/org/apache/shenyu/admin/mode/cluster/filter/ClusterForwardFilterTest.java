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

import com.sun.net.httpserver.HttpServer;
import jakarta.servlet.DispatcherType;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.server.WebServer;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockFilterChain;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;
import org.yaml.snakeyaml.Yaml;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ClusterForwardFilter}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ClusterForwardFilterTest {

    private static final String MASTER_HOST = "10.0.0.9";

    private static final String MASTER_PORT = "9095";

    private static final String MASTER_BODY = "{\"code\":200,\"message\":\"success\",\"data\":{\"id\":\"123\"}}";

    private static HttpServer masterServer;

    private static WebServer followerServer;

    private static int masterPort;

    private static int followerPort;

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

    @BeforeAll
    public static void setUpServers() throws Exception {
        masterServer = HttpServer.create(new InetSocketAddress(0), 0);
        masterServer.createContext("/selector", exchange -> {
            byte[] bytes = MASTER_BODY.getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Content-Type", "application/json");
            // sendResponseHeaders with 0 triggers HTTP/1.1 chunked transfer encoding in JDK HttpServer
            exchange.sendResponseHeaders(200, 0);
            OutputStream os = exchange.getResponseBody();
            os.write(bytes);
            os.close();
        });
        masterServer.start();
        masterPort = masterServer.getAddress().getPort();

        ClusterSelectMasterService selectMasterService = mock(ClusterSelectMasterService.class);
        when(selectMasterService.isMaster()).thenReturn(false);
        ClusterMasterDTO masterDTO = new ClusterMasterDTO();
        masterDTO.setMasterHost("127.0.0.1");
        masterDTO.setMasterPort(String.valueOf(masterPort));
        masterDTO.setContextPath("");
        when(selectMasterService.getMaster()).thenReturn(masterDTO);

        ClusterProperties forwardingProperties = new ClusterProperties();
        forwardingProperties.setForwardList(Collections.singletonList("/**"));
        forwardingProperties.setIgnoredList(Collections.emptyList());

        ClusterForwardFilter filter = new ClusterForwardFilter(new RestTemplate(), selectMasterService, forwardingProperties);

        TomcatServletWebServerFactory factory = new TomcatServletWebServerFactory(0);
        followerServer = factory.getWebServer(servletContext -> {
            servletContext.addFilter("clusterForwardFilter", filter)
                    .addMappingForUrlPatterns(EnumSet.of(DispatcherType.REQUEST), true, "/*");
            servletContext.addServlet("testServlet", new jakarta.servlet.http.HttpServlet() { })
                    .addMapping("/*");
        });
        followerServer.start();
        followerPort = followerServer.getPort();
    }

    @AfterAll
    public static void tearDownServers() {
        if (Objects.nonNull(followerServer)) {
            followerServer.stop();
        }
        if (Objects.nonNull(masterServer)) {
            masterServer.stop(0);
        }
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

    @Test
    public void testCopyRequestHeadersExcludesHopByHopAndHostAndContentLength() {
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader(HttpHeaders.CONNECTION, "close, X-Custom-Hop");
        request.addHeader(HttpHeaders.HOST, "follower.shenyu.apache.org:9096");
        request.addHeader(HttpHeaders.CONTENT_LENGTH, "100");
        request.addHeader(HttpHeaders.TRANSFER_ENCODING, "chunked");
        request.addHeader(HttpHeaders.UPGRADE, "websocket");
        request.addHeader("X-Custom-Hop", "remove-this");
        request.addHeader(HttpHeaders.AUTHORIZATION, "Bearer test-token");
        request.addHeader("X-Forwarded-For", "192.168.1.1");

        HttpHeaders headers = new HttpHeaders();
        ClusterForwardFilter filter = new ClusterForwardFilter(mock(RestTemplate.class));
        filter.copyHeaders(request, headers);

        assertFalse(headers.containsKey(HttpHeaders.CONNECTION));
        assertFalse(headers.containsKey(HttpHeaders.HOST));
        assertFalse(headers.containsKey(HttpHeaders.CONTENT_LENGTH));
        assertFalse(headers.containsKey(HttpHeaders.TRANSFER_ENCODING));
        assertFalse(headers.containsKey(HttpHeaders.UPGRADE));
        assertFalse(headers.containsKey("X-Custom-Hop"));

        assertTrue(headers.containsKey(HttpHeaders.AUTHORIZATION));
        assertEquals("Bearer test-token", headers.getFirst(HttpHeaders.AUTHORIZATION));
        assertTrue(headers.containsKey("X-Forwarded-For"));
        assertEquals("192.168.1.1", headers.getFirst("X-Forwarded-For"));
    }

    @Test
    public void testCopyResponseHeadersExcludesHopByHopAndContentLength() {
        HttpHeaders sourceHeaders = new HttpHeaders();
        sourceHeaders.add(HttpHeaders.TRANSFER_ENCODING, "chunked");
        sourceHeaders.add(HttpHeaders.CONNECTION, "keep-alive, X-Resp-Hop");
        sourceHeaders.add("Keep-Alive", "timeout=5");
        sourceHeaders.add(HttpHeaders.CONTENT_LENGTH, "456");
        sourceHeaders.add("X-Resp-Hop", "strip-this");
        sourceHeaders.add(HttpHeaders.CONTENT_TYPE, "application/json");
        sourceHeaders.add("X-Custom-Header", "allowed-value");

        MockHttpServletResponse response = new MockHttpServletResponse();
        ClusterForwardFilter filter = new ClusterForwardFilter(mock(RestTemplate.class));
        filter.copyHeaders(sourceHeaders, response);

        assertFalse(response.containsHeader(HttpHeaders.TRANSFER_ENCODING));
        assertFalse(response.containsHeader(HttpHeaders.CONNECTION));
        assertFalse(response.containsHeader("Keep-Alive"));
        assertFalse(response.containsHeader(HttpHeaders.CONTENT_LENGTH));
        assertFalse(response.containsHeader("X-Resp-Hop"));

        assertTrue(response.containsHeader(HttpHeaders.CONTENT_TYPE));
        assertEquals("application/json", response.getHeader(HttpHeaders.CONTENT_TYPE));
        assertTrue(response.containsHeader("X-Custom-Header"));
        assertEquals("allowed-value", response.getHeader("X-Custom-Header"));
    }

    @Test
    public void testRealHttpFollowerForwardingWithConnectionClose() throws Exception {
        URI uri = URI.create("http://127.0.0.1:" + followerPort + "/selector");
        HttpURLConnection connection = (HttpURLConnection) uri.toURL().openConnection();
        connection.setRequestMethod("GET");
        connection.setRequestProperty(HttpHeaders.CONNECTION, "close");
        connection.setConnectTimeout(5000);
        connection.setReadTimeout(5000);

        int responseCode = connection.getResponseCode();
        assertEquals(200, responseCode);

        // Verify that follower response does NOT advertise chunked encoding when Connection: close
        assertNull(connection.getHeaderField(HttpHeaders.TRANSFER_ENCODING));

        // Content-Length must be present and match the forwarded payload bytes
        String contentLengthHeader = connection.getHeaderField(HttpHeaders.CONTENT_LENGTH);
        byte[] expectedBytes = MASTER_BODY.getBytes(StandardCharsets.UTF_8);
        assertEquals(String.valueOf(expectedBytes.length), contentLengthHeader);

        // Reading the body must succeed completely without IncompleteRead
        InputStream is = connection.getInputStream();
        byte[] actualBytes = is.readAllBytes();
        assertEquals(new String(expectedBytes, StandardCharsets.UTF_8), new String(actualBytes, StandardCharsets.UTF_8));
        connection.disconnect();
    }

    @Test
    public void testRealHttpFollowerForwardingWithConnectionKeepAlive() throws Exception {
        try (Socket socket = new Socket("127.0.0.1", followerPort)) {
            socket.setSoTimeout(5000);
            OutputStream out = socket.getOutputStream();
            String rawRequest = "GET /selector HTTP/1.1\r\n"
                    + "Host: 127.0.0.1:" + followerPort + "\r\n"
                    + "Connection: keep-alive\r\n\r\n";
            out.write(rawRequest.getBytes(StandardCharsets.UTF_8));
            out.flush();

            BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream(), StandardCharsets.UTF_8));
            String statusLine = reader.readLine();
            assertTrue(statusLine.contains("200"));

            List<String> transferEncodingHeaders = new ArrayList<>();
            List<String> connectionHeaders = new ArrayList<>();
            String line;
            int contentLength = -1;
            while (Objects.nonNull(line = reader.readLine()) && !line.isEmpty()) {
                String lower = line.toLowerCase();
                if (lower.startsWith("transfer-encoding:")) {
                    transferEncodingHeaders.add(line);
                } else if (lower.startsWith("connection:")) {
                    connectionHeaders.add(line);
                } else if (lower.startsWith("content-length:")) {
                    contentLength = Integer.parseInt(line.split(":")[1].trim());
                }
            }

            // Must NOT have duplicate Transfer-Encoding headers
            assertEquals(0, transferEncodingHeaders.size());

            // Connection header must not have duplicated tokens like "keep-alive, keep-alive"
            assertEquals(1, connectionHeaders.size());
            assertFalse(connectionHeaders.get(0).contains("keep-alive, keep-alive"));

            // Content-Length must match the forwarded payload
            byte[] expectedBytes = MASTER_BODY.getBytes(StandardCharsets.UTF_8);
            assertEquals(expectedBytes.length, contentLength);

            char[] bodyChars = new char[contentLength];
            int read = reader.read(bodyChars, 0, contentLength);
            assertEquals(contentLength, read);
            assertEquals(MASTER_BODY, new String(bodyChars));
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
