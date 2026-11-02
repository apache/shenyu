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

import com.google.gson.JsonObject;
import org.springframework.web.util.UriTemplate;

import java.util.Map;
import java.util.Objects;

/**
 * Frozen resource metadata with optional simple URI-template matching.
 * Only simple named variables are supported; no file or network access is performed.
 */
public final class AgentResourceDefinition {

    private final String name;

    private final String uri;

    private final String description;

    private final String mimeType;

    private final UriTemplate template;

    public AgentResourceDefinition(final String name, final String uri, final String description, final String mimeType, final boolean template) {
        this.name = Objects.requireNonNull(name, "name");
        if (name.isBlank() || name.length() > 256 || name.codePoints().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException("Resource name must not be blank");
        }
        this.uri = Objects.requireNonNull(uri, "uri");
        this.description = Objects.requireNonNull(description, "description");
        if (uri.length() > 4096 || description.length() > 4096) {
            throw new IllegalArgumentException("Resource metadata limit exceeded");
        }
        this.mimeType = mimeType;
        this.template = template ? new UriTemplate(uri) : null;
        if (template) {
            if (this.template.getVariableNames().isEmpty() || this.template.getVariableNames().stream().anyMatch(variable -> !variable.matches("[A-Za-z_][A-Za-z0-9_]*"))) {
                throw new IllegalArgumentException("Only simple URI template variables are supported");
            }
            Map<String, String> probes = new java.util.HashMap<>();
            this.template.getVariableNames().forEach(variable -> probes.put(variable, "probe"));
            AgentResourceContent.text(this.template.expand(probes).toString(), mimeType, "");
        } else {
            AgentResourceContent.text(uri, mimeType, "");
        }
    }

    public String getName() {
        return name;
    }

    public boolean isTemplate() {
        return Objects.nonNull(template);
    }

    public String getUri() {
        return uri;
    }

    public java.util.Set<String> getVariables() {
        return isTemplate() ? java.util.Set.copyOf(template.getVariableNames()) : java.util.Set.of();
    }

    /**
     * Match a resource identifier without I/O.
     * @param requested requested absolute identifier
     * @return whether the definition matches
     */
    public boolean matches(final String requested) {
        return isTemplate() ? template.matches(requested) : uri.equals(requested);
    }

    /**
     * Extract immutable simple variables.
     * @param requested already matched URI
     * @return variables, empty for direct resources
     */
    public Map<String, String> variables(final String requested) {
        return isTemplate() ? Map.copyOf(template.match(requested)) : Map.of();
    }

    /**
     * Encode metadata without exposing provider state.
     * @return independent metadata object
     */
    public JsonObject toJson() {
        JsonObject value = new JsonObject();
        value.addProperty("name", name);
        value.addProperty(isTemplate() ? "uriTemplate" : "uri", uri);
        value.addProperty("description", description);
        if (Objects.nonNull(mimeType)) {
            value.addProperty("mimeType", mimeType);
        }
        return value;
    }
}
