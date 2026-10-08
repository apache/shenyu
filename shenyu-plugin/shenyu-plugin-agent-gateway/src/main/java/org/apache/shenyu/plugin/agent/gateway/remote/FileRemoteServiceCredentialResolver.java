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

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

/** Versioned, operator-mounted credential files with independent target binding. */
public final class FileRemoteServiceCredentialResolver implements RemoteServiceCredentialResolver {

    private static final ObjectMapper JSON = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION).enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private final Path directory;

    /**
     * File Remote Service Credential Resolver.
     * @param directory trusted directory value
     */
    public FileRemoteServiceCredentialResolver(final Path directory) {
        try {
            this.directory = directory.toRealPath();
            if (!Files.isDirectory(this.directory)) {
                throw new IllegalArgumentException("Missing credential directory");
            }
        } catch (Exception error) {
            throw new IllegalArgumentException("Cannot access credential directory");
        }
    }

    @Override
    public RemoteServerBinding.Credential resolve(final RemoteServerBinding.Config target) {
        try {
            Path file = directory
                .resolve(target.credentialRef())
                .resolve(target.credentialVersion() + ".json")
                .normalize();
            if (!file.startsWith(directory) || !file.toRealPath().startsWith(directory)) {
                throw new SecurityException("Credential file outside mounted directory");
            }
            JsonNode value;
            try (InputStream input = Files.newInputStream(file)) {
                byte[] bytes = input.readNBytes(4097);
                if (bytes.length > 4096) {
                    throw new SecurityException("Credential file byte limit");
                }
                value = JSON.readTree(bytes);
            }
            Set<String> fields = new HashSet<>();
            value.fieldNames().forEachRemaining(fields::add);
            if (!value.isObject() || !fields.equals(Set.of("name", "endpoint", "credentialRef", "credentialVersion", "bearerToken"))) {
                throw new SecurityException("Invalid credential file fields");
            }
            for (String field : fields) {
                if (!value.get(field).isTextual()) {
                    throw new SecurityException("Invalid credential file value");
                }
            }
            RemoteServerBinding.Config bound = new RemoteServerBinding.Config(
                value.get("name").textValue(),
                URI.create(value.get("endpoint").textValue()),
                value.get("credentialRef").textValue(),
                value.get("credentialVersion").textValue()
            );
            if (!target.equals(bound)) {
                throw new SecurityException("Credential target binding mismatch");
            }
            return new RemoteServerBinding.Credential(bound, value.get("bearerToken").textValue());
        } catch (Exception error) {
            // Never expose raw file contents, parser text, tokens, or a secret-store cause.
            throw new SecurityException("Service credential resolution failed");
        }
    }
}
