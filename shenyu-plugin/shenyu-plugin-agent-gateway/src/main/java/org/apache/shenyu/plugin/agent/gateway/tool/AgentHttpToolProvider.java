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

package org.apache.shenyu.plugin.agent.gateway.tool;

import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.concurrent.TimeoutException;

/**
 * A configured HTTP tool reusing the existing local authorization and result-filter path.
 */
public final class AgentHttpToolProvider implements AgentToolProvider {

    private final AgentGatewayHttpToolConfig.Tool tool;

    private final AgentHttpToolInvoker invoker;

    public AgentHttpToolProvider(final AgentGatewayHttpToolConfig.Tool tool, final AgentHttpToolInvoker invoker) {
        this.tool = Objects.requireNonNull(tool, "tool");
        this.invoker = invoker;
    }

    @Override
    public String getName() {
        return tool.getName();
    }

    @Override
    public String getDescription() {
        return tool.getDescription();
    }

    @Override
    public JsonObject getInputSchema() {
        return tool.getInputSchema();
    }

    @Override
    public void validate(final JsonObject arguments) {
        tool.validate(arguments);
    }

    @Override
    public Mono<JsonObject> invoke(final AgentToolInvocation invocation) {
        return Mono.defer(() -> {
            if (Objects.isNull(invoker)) {
                return Mono.error(new IllegalStateException("Internal HTTP dispatch is not installed"));
            }
            return invoker.invoke(tool, invocation).onErrorMap(TimeoutException.class, error -> new DeadlineExceededException());
        });
    }

    /**
     * HTTP-specific deadline marker; other providers retain their existing error classification.
     */
    public static final class DeadlineExceededException extends RuntimeException {

        private static final long serialVersionUID = 1L;

        private DeadlineExceededException() {
            super("HTTP tool deadline exceeded");
        }
    }
}
