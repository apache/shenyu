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

package org.apache.shenyu.plugin.ai.proxy.enhanced.provider;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.github.agenticai.connect.Connect;
import io.github.agenticai.connect.ConversionContext;
import io.github.agenticai.connect.ConversionException;
import io.github.agenticai.connect.ConversionResult;
import io.github.agenticai.connect.Diagnostic;
import io.github.agenticai.connect.Format;
import io.github.agenticai.connect.Json;
import io.github.agenticai.connect.Options;
import io.github.agenticai.connect.ProviderProfile;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Objects;

/**
 * Applies a provider-specific dialect to an OpenAI Chat request.
 */
public final class ConnectProviderDialectAdapter {

    private static final Logger LOG = LoggerFactory.getLogger(ConnectProviderDialectAdapter.class);

    private static final ObjectMapper MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private static final Connect CONNECT = new Connect();

    private final ConversionContext context;

    /**
     * Create a request-scoped provider dialect adapter.
     *
     * @param upstreamModel authoritative routed model, or null to retain the request model
     * @param profile explicitly selected provider dialect, or null for generic Chat
     */
    public ConnectProviderDialectAdapter(final String upstreamModel, final ProviderProfile profile) {
        Options options = new Options();
        options.toolLossPolicy = Options.LossPolicy.SAFE;
        options.providerProfile = profile;
        context = new ConversionContext(options);
        context.upstreamModel = Objects.requireNonNullElse(upstreamModel, "");
    }

    /**
     * Apply the provider dialect without changing the Chat protocol.
     *
     * @param payload standard OpenAI Chat payload
     * @return provider-specific Chat payload
     */
    public ObjectNode adapt(final ObjectNode payload) {
        Objects.requireNonNull(payload, "AI provider payload is required");
        try {
            ConversionResult result = CONNECT.convertRequest(Format.OPENAI_CHAT, Format.OPENAI_CHAT,
                    Json.object(payload.toString()), context);
            reportDiagnostics(result.diagnostics());
            return (ObjectNode) MAPPER.readTree(result.json());
        } catch (Exception error) {
            if (error instanceof ConversionException conversionException) {
                reportDiagnostics(conversionException.diagnostics());
            }
            throw new AiProxyException(AiProxyException.Kind.REQUEST,
                    "AI provider dialect adaptation failed", error);
        }
    }

    private void reportDiagnostics(final List<Diagnostic> diagnostics) {
        for (Diagnostic diagnostic : diagnostics) {
            // Never log payloads, credentials, private reasoning or library diagnostic messages.
            LOG.warn("AI provider dialect diagnostic: code={}, severity={}, from={}, to={}",
                    diagnostic.code(), diagnostic.severity(), diagnostic.from(), diagnostic.to());
        }
    }
}
