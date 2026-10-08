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

package org.apache.shenyu.plugin.agent.gateway.remote;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.net.URI;
import java.net.Proxy;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

class RemoteTransportPolicyTest {

    @ParameterizedTest
    @ValueSource(strings = {"https://192.0.2.10:443/mcp", "https://10.1.2.3:8443/tools/mcp", "http://127.0.0.1:8080/mcp"})
    void acceptsOnlyUnambiguousFixedIpTargets(final String endpoint) {
        assertDoesNotThrow(() -> RemoteTransportPolicy.endpoint(URI.create(endpoint)));
    }

    @ParameterizedTest
    @ValueSource(strings = {"https://example.com:443/mcp", "http://192.0.2.10:8080/mcp", "https://0.0.0.0:443/mcp",
        "https://169.254.169.254:443/mcp", "https://224.1.2.3:443/mcp", "https://256.1.2.3:443/mcp", "https://010.1.2.3:443/mcp",
        "https://[::1]:443/mcp", "https://127.0.0.1/mcp", "https://user@127.0.0.1:443/mcp", "https://127.0.0.1:443/mcp?x=1",
        "https://127.0.0.1:443/a/../mcp", "https://127.0.0.1:443/%2e/mcp", "https://127.0.0.1:443//mcp"})
    void rejectsDnsPlaintextRemoteAndAmbiguousAddresses(final String endpoint) {
        assertThrows(IllegalArgumentException.class, () -> RemoteTransportPolicy.endpoint(URI.create(endpoint)));
    }

    @Test
    void neverUsesJvmProxyAndBindsCredentialToExactTarget() {
        URI endpoint = URI.create("https://192.0.2.10:443/mcp");
        assertEquals(List.of(Proxy.NO_PROXY), RemoteTransportPolicy.directOnly().select(endpoint));
        RemoteServerBinding.Config config = new RemoteServerBinding.Config("orders", endpoint, "service/orders", "v1");
        RemoteServerBinding.Config other = new RemoteServerBinding.Config("orders", URI.create("https://192.0.2.10:443/other"), "service/orders", "v1");
        assertThrows(SecurityException.class, () -> RemoteServerBinding.resolve(config, Set.of(endpoint), ignored -> new RemoteServerBinding.Credential(other, "fake")));
        assertThrows(SecurityException.class, () -> RemoteServerBinding.resolve(config, Set.of(), ignored -> new RemoteServerBinding.Credential(config, "fake")));
        SecurityException error = assertThrows(SecurityException.class, () -> RemoteServerBinding.resolve(config, Set.of(endpoint), ignored -> {
            throw new IllegalStateException("SECRET");
        }));
        assertEquals(null, error.getCause());
        assertEquals("ServiceCredential[REDACTED]", new RemoteServerBinding.Credential(config, "fake").toString());
    }

    @Test
    void rejectsDelegatedMetadataButKeepsOpaqueResultMetadata() {
        ObjectMapper json = new ObjectMapper();
        var request = json.createObjectNode();
        request.putObject("_meta").put("progressToken", "SECRET");
        assertThrows(IllegalArgumentException.class, () -> RemoteTransportPolicy.callMetadata(request));
        var result = json.createObjectNode();
        result.putObject("_meta").put("Authorization", "opaque-result-data");
        RemoteTransportPolicy.resultMetadata(result);
        assertEquals("opaque-result-data", result.path("_meta").path("Authorization").textValue());
        assertThrows(IllegalArgumentException.class, () -> RemoteTransportPolicy.session(List.of("one", "two")));
    }
}
