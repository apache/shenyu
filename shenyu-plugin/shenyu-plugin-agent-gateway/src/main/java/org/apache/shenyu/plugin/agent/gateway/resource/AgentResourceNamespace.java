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

package org.apache.shenyu.plugin.agent.gateway.resource;

import org.springframework.web.util.UriUtils;

import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Reversible resource namespace; identifiers are never network destinations.
 * URI templates support only simple named variables, as local providers do.
 */
public final class AgentResourceNamespace {

    private static final Pattern VARIABLE = Pattern.compile("\\{[A-Za-z_][A-Za-z0-9_]*\\}");

    private AgentResourceNamespace() {
    }

    /**
     * Prefix a complete upstream identifier or simple template.
     * @param server fixed configured server name
     * @param original upstream identifier
     * @param template whether simple variables must remain expandable
     * @return namespaced identifier
     */
    public static String expose(final String server, final String original, final boolean template) {
        if (Objects.isNull(server) || !server.matches("[A-Za-z0-9_-]{1,48}") || Objects.isNull(original) || original.length() > 4096) {
            throw new IllegalArgumentException("Invalid resource namespace");
        }
        StringBuilder result = new StringBuilder("mcp-resource://").append(server).append('/');
        Matcher variables = VARIABLE.matcher(original);
        int offset = 0;
        while (template && variables.find()) {
            result.append(UriUtils.encode(original.substring(offset, variables.start()), StandardCharsets.UTF_8));
            result.append(variables.group());
            offset = variables.end();
        }
        result.append(UriUtils.encode(original.substring(offset), StandardCharsets.UTF_8));
        return result.toString();
    }

    /**
     * Recover an already authorized identifier for the same configured server.
     * @param server fixed target name
     * @param exposed matched namespaced identifier
     * @return canonical upstream identifier
     */
    public static String original(final String server, final String exposed) {
        String prefix = "mcp-resource://" + server + "/";
        if (!exposed.startsWith(prefix)) {
            throw new IllegalArgumentException("Resource target mismatch");
        }
        String original = UriUtils.decode(exposed.substring(prefix.length()), StandardCharsets.UTF_8);
        AgentResourceContent.text(original, null, "");
        if (!expose(server, original, false).equals(exposed)) {
            throw new IllegalArgumentException("Noncanonical resource identifier");
        }
        return original;
    }
}
