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

package org.apache.shenyu.web.diagnostics;

import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import java.time.Duration;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link ShenyuDiagnosticsEndpoint}.
 */
public final class ShenyuDiagnosticsEndpointTest {

    @Test
    public void bundleReturnsAZipAttachment() {
        DiagnosticProperties properties = new DiagnosticProperties();
        DiagnosticBundleService service = new DiagnosticBundleService(
                List.of(new RuntimeInfoCollector()), properties, new DiagnosticBundleWriter(properties));
        ShenyuDiagnosticsEndpoint endpoint = new ShenyuDiagnosticsEndpoint(service);

        ResponseEntity<byte[]> response = endpoint.bundle().block(Duration.ofSeconds(10));

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(MediaType.APPLICATION_OCTET_STREAM, response.getHeaders().getContentType());
        String disposition = response.getHeaders().getFirst(HttpHeaders.CONTENT_DISPOSITION);
        assertTrue(disposition.startsWith("attachment; filename=\"shenyu-diagnostics-"));
        byte[] body = response.getBody();
        assertNotNull(body);
        assertTrue(body.length > 0);
        assertEquals((byte) 'P', body[0]);
        assertEquals((byte) 'K', body[1]);
    }
}
