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

package org.apache.shenyu.springboot.starter.sync.data.file;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.springframework.core.env.Environment;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Strict version-one HTTP schema. Validate the entire document before invoking any subscriber.
 * Diagnostics intentionally contain schema paths, never configuration values or parser excerpts.
 */
final class FileConfigurationParser {

    private static final Set<String> SUPPORTED = Set.of("global", "divide", "uri", "nettyHttpClient", "response", "contextPath");

    private static final Set<String> REQUIRED = Set.of("global", "divide", "uri", "nettyHttpClient", "response");

    private static final int MAX_BYTES = 1024 * 1024;

    private final ObjectMapper mapper = new ObjectMapper(new YAMLFactory().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION));

    private final Environment environment;

    private final String namespace;

    private final Map<String, ShenyuPlugin> available = new LinkedHashMap<>();

    private final Set<String> handlers;

    private final Set<String> upstreamHandlers;

    FileConfigurationParser(final Environment environment, final String namespace, final List<ShenyuPlugin> plugins,
                            final Set<String> handlers, final Set<String> upstreamHandlers) {
        this.environment = environment;
        this.namespace = namespace;
        this.handlers = handlers;
        this.upstreamHandlers = upstreamHandlers;
        for (ShenyuPlugin plugin : plugins) {
            require(!available.containsKey(plugin.named()), "plugins", "duplicate plugin implementation on the classpath");
            available.put(plugin.named(), plugin);
        }
    }

    Snapshot read(final Path path) {
        require(Files.isRegularFile(path), "$", "cannot read external configuration file: expected a regular file");
        JsonNode root;
        try (InputStream input = Files.newInputStream(path)) {
            byte[] bytes = input.readNBytes(MAX_BYTES + 1);
            require(bytes.length <= MAX_BYTES, "$", "file exceeds 1 MiB");
            try (JsonParser parser = mapper.createParser(bytes)) {
                root = mapper.readTree(parser);
                require(Objects.isNull(parser.nextToken()), "$", "only one document is supported");
            }
        } catch (JsonProcessingException ex) {
            throw invalid("$", "malformed YAML/JSON at line " + ex.getLocation().getLineNr() + ", column " + ex.getLocation().getColumnNr());
        } catch (IOException ex) {
            throw invalid("$", "cannot read external configuration file");
        }
        fields(root, "$", Set.of("version", "plugins", "selectors", "rules", "upstreams", "metadata", "auth"));
        require(integer(root, "version", "$", 1, 1, -1) == 1, "$.version", "version 1 is required");
        for (String unsupported : List.of("metadata", "auth")) {
            JsonNode entries = root.path(unsupported);
            require(entries.isMissingNode() || entries.isArray() && entries.isEmpty(), "$." + unsupported, "unsupported in version 1; omit or use an empty array");
        }
        List<PluginData> plugins = plugins(root);
        Map<String, PluginData> configured = new LinkedHashMap<>();
        plugins.forEach(plugin -> configured.put(plugin.getName(), plugin));
        List<SelectorData> selectors = selectors(root, configured);
        Map<String, SelectorData> byId = new LinkedHashMap<>();
        selectors.forEach(selector -> byId.put(selector.getId(), selector));
        List<RuleData> rules = rules(root, byId);
        List<DiscoverySyncData> upstreams = upstreams(root, byId);
        for (SelectorData selector : selectors) {
            require(rules.stream().anyMatch(rule -> rule.getSelectorId().equals(selector.getId())), "selectors", "every selector requires a rule");
            if ("divide".equals(selector.getPluginName())) {
                require(upstreams.stream().anyMatch(upstream -> upstream.getSelectorId().equals(selector.getId())), "upstreams", "every divide selector requires static upstreams");
            }
        }
        return new Snapshot(List.copyOf(plugins), List.copyOf(selectors), List.copyOf(rules), List.copyOf(upstreams));
    }

    private List<PluginData> plugins(final JsonNode root) {
        List<PluginData> result = new ArrayList<>();
        Set<String> names = new HashSet<>();
        for (JsonNode entry : array(root, "plugins", "$")) {
            String at = "plugins[" + result.size() + "]";
            fields(entry, at, Set.of("name", "enabled", "sort", "config"));
            String name = text(entry, "name", at);
            require(names.add(name), at, "duplicate plugin name");
            require(SUPPORTED.contains(name), at + ".name", "unsupported HTTP plugin");
            require(available.containsKey(name), at + ".name", "required plugin is missing from the classpath");
            int order = available.get(name).getOrder();
            require(integer(entry, "sort", at, Integer.MIN_VALUE, Integer.MAX_VALUE, order) == order, at + ".sort", "must match the plugin's built-in order");
            JsonNode config = entry.path("config");
            require(config.isMissingNode() || config.isObject() && config.isEmpty(), at + ".config", "this plugin has no global configuration; use runtime settings or rule handles");
            boolean enabled = bool(entry, "enabled", at, true);
            require(!REQUIRED.contains(name) || enabled, at + ".enabled", "required HTTP infrastructure must be enabled");
            result.add(new PluginData("file-plugin-" + name, name, "{}", "", enabled, order, "", namespace));
        }
        require(names.containsAll(REQUIRED), "plugins", "global, divide, uri, nettyHttpClient and response must be explicitly declared");
        require(handlers.contains("divide") && upstreamHandlers.contains("divide"), "plugins", "divide configuration and upstream handlers are required");
        require(!names.contains("contextPath") || handlers.contains("contextPath"), "plugins", "contextPath configuration handler is required");
        // The handler starts with all classpath plugins. Explicitly disable undeclared ones.
        available.forEach((name, plugin) -> {
            if (!names.contains(name)) {
                result.add(new PluginData("file-plugin-" + name, name, "{}", "", false, plugin.getOrder(), "", namespace));
            }
        });
        return result;
    }

    private List<SelectorData> selectors(final JsonNode root, final Map<String, PluginData> plugins) {
        List<SelectorData> result = new ArrayList<>();
        Set<String> ids = new HashSet<>();
        Set<String> orders = new HashSet<>();
        for (JsonNode entry : array(root, "selectors", "$")) {
            String at = "selectors[" + result.size() + "]";
            fields(entry, at, Set.of("id", "plugin", "name", "enabled", "sort", "matchMode", "conditions"));
            String id = identity(entry, at, ids);
            String plugin = text(entry, "plugin", at);
            require(Set.of("divide", "contextPath").contains(plugin) && plugins.containsKey(plugin), at + ".plugin", "selector must reference a declared routing plugin");
            int sort = integer(entry, "sort", at, 0, Integer.MAX_VALUE, 0);
            require(orders.add(plugin + ":" + sort), at + ".sort", "selector order must be unique within its plugin");
            List<ConditionData> conditions = conditions(entry, at);
            SelectorData selector = SelectorData.builder().id(id).name(optionalText(entry, "name", at, id))
                    .pluginId("file-plugin-" + plugin).pluginName(plugin).enabled(bool(entry, "enabled", at, true))
                    .sort(sort).type(SelectorTypeEnum.CUSTOM_FLOW.getCode()).matchMode(matchMode(entry, at))
                    .logged(false).continued(true).matchRestful(false).handle("{}").conditionList(conditions).namespaceId(namespace).build();
            result.add(selector);
        }
        return result;
    }

    private List<RuleData> rules(final JsonNode root, final Map<String, SelectorData> selectors) {
        List<RuleData> result = new ArrayList<>();
        Set<String> ids = new HashSet<>();
        Set<String> orders = new HashSet<>();
        for (JsonNode entry : array(root, "rules", "$")) {
            String at = "rules[" + result.size() + "]";
            fields(entry, at, Set.of("id", "selector", "name", "enabled", "sort", "matchMode", "conditions", "handle"));
            String id = identity(entry, at, ids);
            String selectorId = text(entry, "selector", at);
            require(selectors.containsKey(selectorId), at + ".selector", "unknown selector reference");
            String plugin = selectors.get(selectorId).getPluginName();
            int sort = integer(entry, "sort", at, 0, Integer.MAX_VALUE, 0);
            require(orders.add(selectorId + ":" + sort), at + ".sort", "rule order must be unique within its selector");
            String handle = handle(entry.path("handle"), plugin, at + ".handle");
            result.add(RuleData.builder().id(id).name(optionalText(entry, "name", at, id)).selectorId(selectorId).pluginName(plugin)
                    .enabled(bool(entry, "enabled", at, true)).sort(sort).matchMode(matchMode(entry, at)).loged(false).matchRestful(false)
                    .handle(handle).conditionDataList(conditions(entry, at)).namespaceId(namespace).build());
        }
        return result;
    }

    private String handle(final JsonNode node, final String plugin, final String at) {
        if ("contextPath".equals(plugin)) {
            fields(node, at, Set.of("contextPath", "addPrefix"));
            require(node.has("contextPath") || node.has("addPrefix"), at, "contextPath or addPrefix is required");
            node.fieldNames().forEachRemaining(key -> require(text(node, key, at).startsWith("/"), at + "." + key, "must start with a slash"));
        } else {
            fields(node, at, Set.of("loadBalance", "retry", "timeout", "headerMaxSize", "requestMaxSize"));
            String balance = optionalText(node, "loadBalance", at, "random");
            require(Set.of("random", "roundRobin").contains(balance), at + ".loadBalance", "version 1 supports random or roundRobin");
            integer(node, "retry", at, 0, 10, 3);
            integer(node, "timeout", at, 1, Integer.MAX_VALUE, 3000);
            integer(node, "headerMaxSize", at, 0, Integer.MAX_VALUE, 0);
            integer(node, "requestMaxSize", at, 0, Integer.MAX_VALUE, 0);
        }
        return node.toString();
    }

    private List<DiscoverySyncData> upstreams(final JsonNode root, final Map<String, SelectorData> selectors) {
        List<DiscoverySyncData> result = new ArrayList<>();
        Set<String> references = new HashSet<>();
        for (JsonNode entry : array(root, "upstreams", "$")) {
            String at = "upstreams[" + result.size() + "]";
            fields(entry, at, Set.of("selector", "targets"));
            String selectorId = text(entry, "selector", at);
            require(selectors.containsKey(selectorId) && "divide".equals(selectors.get(selectorId).getPluginName()), at + ".selector", "must reference a divide selector");
            require(references.add(selectorId), at + ".selector", "duplicate upstream selector reference");
            List<DiscoveryUpstreamData> targets = new ArrayList<>();
            Set<String> urls = new HashSet<>();
            for (JsonNode target : array(entry, "targets", at)) {
                String targetAt = at + ".targets[" + targets.size() + "]";
                fields(target, targetAt, Set.of("url", "weight"));
                URI uri;
                try {
                    uri = URI.create(text(target, "url", targetAt));
                } catch (IllegalArgumentException ex) {
                    throw invalid(targetAt + ".url", "invalid HTTP origin");
                }
                require(("http".equals(uri.getScheme()) || "https".equals(uri.getScheme())) && Objects.nonNull(uri.getHost())
                        && Objects.isNull(uri.getUserInfo()) && Objects.isNull(uri.getRawQuery()) && Objects.isNull(uri.getRawFragment())
                        && (uri.getPath().isEmpty() || "/".equals(uri.getPath())) && uri.getPort() >= -1 && uri.getPort() != 0 && uri.getPort() <= 65535,
                        targetAt + ".url", "must be an HTTP(S) origin without credentials, path, query or fragment");
                String authority = uri.getHost().toLowerCase(Locale.ROOT) + ":" + (uri.getPort() == -1 ? ("https".equals(uri.getScheme()) ? 443 : 80) : uri.getPort());
                require(urls.add(uri.getScheme() + "://" + authority), targetAt + ".url", "duplicate upstream target");
                DiscoveryUpstreamData data = new DiscoveryUpstreamData();
                data.setId(selectorId + "-upstream-" + targets.size());
                data.setNamespaceId(namespace);
                data.setProtocol(uri.getScheme() + "://");
                data.setUrl(authority);
                data.setWeight(integer(target, "weight", targetAt, 1, 10000, 100));
                data.setStatus(0);
                data.setDateCreated(new Timestamp(0));
                data.setProps("{\"warmup\":\"0\",\"healthCheckEnabled\":\"false\"}");
                targets.add(data);
            }
            require(!targets.isEmpty(), at + ".targets", "at least one static target is required");
            DiscoverySyncData data = new DiscoverySyncData();
            data.setNamespaceId(namespace);
            data.setPluginName("divide");
            data.setSelectorId(selectorId);
            data.setSelectorName(selectors.get(selectorId).getName());
            data.setUpstreamDataList(targets);
            result.add(data);
        }
        return result;
    }

    private List<ConditionData> conditions(final JsonNode entry, final String at) {
        List<ConditionData> result = new ArrayList<>();
        for (JsonNode node : array(entry, "conditions", at)) {
            String conditionAt = at + ".conditions[" + result.size() + "]";
            fields(node, conditionAt, Set.of("paramType", "operator", "paramName", "paramValue"));
            String type = text(node, "paramType", conditionAt);
            String operator = text(node, "operator", conditionAt);
            require(Set.of("uri", "req_method", "header", "query").contains(type), conditionAt + ".paramType", "unsupported condition parameter type");
            require(Set.of("=", "match", "startsWith", "endsWith", "contains", "regex").contains(operator), conditionAt + ".operator", "unsupported condition operator");
            String value = text(node, "paramValue", conditionAt);
            if ("regex".equals(operator)) {
                try {
                    Pattern.compile(value);
                } catch (PatternSyntaxException ex) {
                    throw invalid(conditionAt + ".paramValue", "invalid regular expression");
                }
            }
            ConditionData data = new ConditionData();
            data.setParamType(type);
            data.setOperator(operator);
            data.setParamName(Set.of("header", "query").contains(type) ? text(node, "paramName", conditionAt) : optionalText(node, "paramName", conditionAt, "/"));
            data.setParamValue(value);
            result.add(data);
        }
        require(!result.isEmpty(), at + ".conditions", "at least one condition is required; use uri match /** for all requests");
        return result;
    }

    private int matchMode(final JsonNode node, final String at) {
        String mode = optionalText(node, "matchMode", at, "and");
        require(Set.of("and", "or").contains(mode), at + ".matchMode", "must be and or or");
        return "and".equals(mode) ? 0 : 1;
    }

    private String identity(final JsonNode node, final String at, final Set<String> ids) {
        String id = text(node, "id", at);
        require(id.matches("[A-Za-z0-9][A-Za-z0-9_.-]{0,127}"), at + ".id", "use 1-128 letters, digits, underscores, dots or hyphens");
        require(ids.add(id), at + ".id", "duplicate identity");
        return id;
    }

    private void fields(final JsonNode node, final String at, final Set<String> allowed) {
        require(Objects.nonNull(node) && node.isObject(), at, "expected an object");
        node.fieldNames().forEachRemaining(field -> require(allowed.contains(field), at, "unknown field; consult the version 1 schema"));
    }

    private JsonNode array(final JsonNode node, final String name, final String at) {
        JsonNode value = node.path(name);
        require(value.isArray(), at + "." + name, "expected an array");
        return value;
    }

    private String optionalText(final JsonNode node, final String name, final String at, final String fallback) {
        return node.has(name) ? text(node, name, at) : fallback;
    }

    private String text(final JsonNode node, final String name, final String at) {
        JsonNode value = node.path(name);
        require(value.isTextual(), at + "." + name, "expected a string");
        String resolved;
        try {
            resolved = environment.resolveRequiredPlaceholders(value.textValue());
        } catch (IllegalArgumentException ex) {
            throw invalid(at + "." + name, "unresolved environment placeholder");
        }
        require(!resolved.isBlank(), at + "." + name, "must not be blank");
        // Resolve after parsing so a placeholder value cannot change the YAML structure.
        ((ObjectNode) node).put(name, resolved);
        return resolved;
    }

    private int integer(final JsonNode node, final String name, final String at, final int min, final int max, final int fallback) {
        if (!node.has(name)) {
            return fallback;
        }
        JsonNode value = node.path(name);
        require(value.isIntegralNumber() && value.canConvertToInt() && value.intValue() >= min && value.intValue() <= max,
                at + "." + name, "expected an integer between " + min + " and " + max);
        return value.intValue();
    }

    private boolean bool(final JsonNode node, final String name, final String at, final boolean fallback) {
        if (!node.has(name)) {
            return fallback;
        }
        require(node.path(name).isBoolean(), at + "." + name, "expected a boolean");
        return node.path(name).booleanValue();
    }

    private static void require(final boolean valid, final String at, final String message) {
        if (!valid) {
            throw invalid(at, message);
        }
    }

    private static IllegalStateException invalid(final String at, final String message) {
        return new IllegalStateException("Invalid file configuration at " + at + ": " + message);
    }

    /**
     * Fully validated configuration, ready for ordered subscriber application.
     * @param plugins explicit enablement of every available plugin
     * @param selectors selector data
     * @param rules rule data
     * @param upstreams static HTTP upstream data
     */
    record Snapshot(List<PluginData> plugins, List<SelectorData> selectors, List<RuleData> rules, List<DiscoverySyncData> upstreams) {
    }
}
