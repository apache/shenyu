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
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class FileRemoteServiceCredentialResolverTest {

    @TempDir
    private Path root;

    private final RemoteServerBinding.Config target = new RemoteServerBinding.Config("orders", URI.create("https://192.0.2.10:443/mcp"), "service/orders", "v1");

    @Test
    void readsIndependentlyBoundVersionWithoutDisplayingToken() throws Exception {
        Files.createDirectories(root.resolve("service/orders"));
        Files.writeString(root.resolve("service/orders/v1.json"), document());
        FileRemoteServiceCredentialResolver resolver = new FileRemoteServiceCredentialResolver(root);
        assertEquals("ServiceCredential[REDACTED]", resolver.resolve(target).toString());
        RemoteServerBinding.Config next = new RemoteServerBinding.Config("orders", target.endpoint(), "service/orders", "v2");
        assertThrows(SecurityException.class, () -> resolver.resolve(next));
    }

    @ParameterizedTest
    @ValueSource(strings = {"name", "endpoint", "credentialRef", "credentialVersion", "bearerToken"})
    void rejectsMissingFieldsWithoutSecretCause(final String field) throws Exception {
        var json = new ObjectMapper();
        var document = (com.fasterxml.jackson.databind.node.ObjectNode) json.readTree(document());
        document.remove(field);
        Files.createDirectories(root.resolve("service/orders"));
        Files.writeString(root.resolve("service/orders/v1.json"), document.toString());
        SecurityException error = assertThrows(SecurityException.class, () -> new FileRemoteServiceCredentialResolver(root).resolve(target));
        assertEquals(null, error.getCause());
        assertEquals("Service credential resolution failed", error.getMessage());
    }

    @Test
    void rejectsWrongEndpointEvenWhenReferenceMatches() throws Exception {
        Files.createDirectories(root.resolve("service/orders"));
        Files.writeString(root.resolve("service/orders/v1.json"), document().replace("192.0.2.10", "192.0.2.11"));
        assertThrows(SecurityException.class, () -> new FileRemoteServiceCredentialResolver(root).resolve(target));
    }

    @Test
    void rejectsOversizeDuplicateAndTraversalFiles() throws Exception {
        Files.createDirectories(root.resolve("service/orders"));
        Path file = root.resolve("service/orders/v1.json");
        Files.writeString(file, "x".repeat(4097));
        FileRemoteServiceCredentialResolver resolver = new FileRemoteServiceCredentialResolver(root);
        assertThrows(SecurityException.class, () -> resolver.resolve(target));
        Files.writeString(file, document().replace("\"name\":\"orders\"", "\"name\":\"orders\",\"name\":\"other\""));
        assertThrows(SecurityException.class, () -> resolver.resolve(target));
        RemoteServerBinding.Config traversal = new RemoteServerBinding.Config("orders", target.endpoint(), "../outside", "v1");
        assertThrows(SecurityException.class, () -> resolver.resolve(traversal));
    }

    private String document() {
        return "{\"name\":\"orders\",\"endpoint\":\"https://192.0.2.10:443/mcp\",\"credentialRef\":\"service/orders\","
                + "\"credentialVersion\":\"v1\",\"bearerToken\":\"fake-unit-token\"}";
    }
}
