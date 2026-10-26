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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.mock.env.MockEnvironment;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.stream.IntStream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Validate schema failures before any publication, including sensitive-value diagnostics.
 */
class FileConfigurationParserTest {

    @TempDir
    private Path directory;

    static List<ShenyuPlugin> plugins() {
        List<String> names = List.of("global", "contextPath", "divide", "uri", "nettyHttpClient", "response");
        List<Integer> orders = List.of(-1, 80, 200, 205, 210, 420);
        return IntStream.range(0, names.size()).mapToObj(index -> {
            ShenyuPlugin plugin = mock(ShenyuPlugin.class);
            when(plugin.named()).thenReturn(names.get(index));
            when(plugin.getOrder()).thenReturn(orders.get(index));
            return plugin;
        }).toList();
    }

    static ObjectNode document() throws IOException {
        try (var input = FileConfigurationParserTest.class.getResourceAsStream("/gateway.yaml")) {
            return (ObjectNode) new ObjectMapper(new YAMLFactory()).readTree(input);
        }
    }

    private FileConfigurationParser parser() {
        return new FileConfigurationParser(new MockEnvironment(), "namespace", plugins(), Set.of("divide", "contextPath"), Set.of("divide"));
    }

    private Path write(final String content) throws IOException {
        Path path = directory.resolve("gateway.yaml");
        Files.writeString(path, content);
        return path;
    }

    @Test
    void restoresDeterministicIdentitiesAndResolvesPlaceholders() throws IOException {
        MockEnvironment environment = new MockEnvironment().withProperty("UPSTREAM_ORIGIN", "https://localhost:8443");
        FileConfigurationParser parser = new FileConfigurationParser(environment, "namespace", plugins(), Set.of("divide", "contextPath"), Set.of("divide"));
        Path path = write(document().toString());
        FileConfigurationParser.Snapshot first = parser.read(path);
        FileConfigurationParser.Snapshot second = parser.read(path);
        assertThat(first.plugins()).isEqualTo(second.plugins());
        assertThat(first.selectors()).isEqualTo(second.selectors());
        assertThat(first.rules()).isEqualTo(second.rules());
        assertThat(first.selectors()).allSatisfy(selector -> assertThat(selector.getLogged()).isFalse());
        assertThat(first.rules()).allSatisfy(rule -> assertThat(rule.getLogged()).isFalse());
        assertThat(first.upstreams().get(0).getUpstreamDataList()).isEqualTo(second.upstreams().get(0).getUpstreamDataList());
        assertThat(first.upstreams().get(0).getUpstreamDataList().get(0).getUrl()).isEqualTo("localhost:8443");
        assertThat(first.upstreams().get(0).getUpstreamDataList().get(0).getProtocol()).isEqualTo("https://");
    }

    @ParameterizedTest
    @ValueSource(strings = {"unknownField", "version", "missingPlugins", "pluginMissing", "duplicatePlugin", "pluginOrder", "pluginConfig", "disabledCore",
        "duplicateSelector", "unknownPlugin", "duplicateRule", "unknownSelector", "ruleOrder", "badHandle", "badTimeout", "badRetry", "badBalance", "badRegex", "badCondition",
        "emptyConditions", "badBoolean", "upstreamReference", "duplicateTargets", "badUpstream", "credentials", "metadata", "auth", "placeholder", "emptyTargets", "noRule", "noUpstream"})
    void rejectsInvalidDocuments(final String scenario) throws IOException {
        ObjectNode node = document();
        ObjectNode plugin = (ObjectNode) node.path("plugins").get(0);
        ObjectNode selector = (ObjectNode) node.path("selectors").get(0);
        ObjectNode rule = (ObjectNode) node.path("rules").get(0);
        ObjectNode target = (ObjectNode) node.path("upstreams").get(0).path("targets").get(0);
        switch (scenario) {
            case "unknownField" -> node.put("sensitive-unknown", "super-secret");
            case "version" -> node.put("version", 2);
            case "missingPlugins" -> node.putArray("plugins");
            case "pluginMissing" -> plugin.put("name", "unavailable");
            case "duplicatePlugin" -> node.withArray("plugins").add(plugin.deepCopy());
            case "pluginOrder" -> plugin.put("sort", 999);
            case "pluginConfig" -> plugin.putObject("config").put("password", "super-secret");
            case "disabledCore" -> plugin.put("enabled", false);
            case "duplicateSelector" -> node.withArray("selectors").add(selector.deepCopy());
            case "unknownPlugin" -> selector.put("plugin", "response");
            case "duplicateRule" -> node.withArray("rules").add(rule.deepCopy());
            case "unknownSelector" -> rule.put("selector", "missing");
            case "ruleOrder" -> node.withArray("rules").add(rule.deepCopy().put("id", "other"));
            case "badHandle" -> ((ObjectNode) rule.path("handle")).put("unrecognized", "super-secret");
            case "badTimeout" -> ((ObjectNode) rule.path("handle")).put("timeout", -1);
            case "badRetry" -> ((ObjectNode) rule.path("handle")).put("retry", "super-secret");
            case "badBalance" -> ((ObjectNode) rule.path("handle")).put("loadBalance", "missing");
            case "badRegex" -> ((ObjectNode) rule.path("conditions").get(0)).put("operator", "regex").put("paramValue", "[super-secret");
            case "badCondition" -> ((ObjectNode) rule.path("conditions").get(0)).put("paramType", "unsupported");
            case "emptyConditions" -> selector.putArray("conditions");
            case "badBoolean" -> selector.put("enabled", "super-secret");
            case "upstreamReference" -> ((ObjectNode) node.path("upstreams").get(0)).put("selector", "missing");
            case "duplicateTargets" -> ((ObjectNode) node.path("upstreams").get(0)).withArray("targets").add(target.deepCopy());
            case "badUpstream" -> target.put("url", "no-scheme-super-secret");
            case "credentials" -> target.put("url", "http://user:super-secret@localhost:8080");
            case "metadata" -> node.withArray("metadata").addObject().put("path", "super-secret");
            case "auth" -> node.withArray("auth").addObject().put("secret", "super-secret");
            case "placeholder" -> target.put("url", "${MISSING_SECRET}");
            case "emptyTargets" -> ((ObjectNode) node.path("upstreams").get(0)).putArray("targets");
            case "noRule" -> node.putArray("rules");
            case "noUpstream" -> node.putArray("upstreams");
            default -> throw new IllegalArgumentException(scenario);
        }
        Path path = write(node.toString());
        assertThatThrownBy(() -> parser().read(path)).isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("Invalid file configuration").hasMessageNotContaining("super-secret").hasNoCause();
    }

    @ParameterizedTest
    @ValueSource(strings = {"version: [super-secret", "version: 1\nversion: 2", "---\nversion: 1\n---\nversion: 1", "[]", "", "null"})
    void rejectsMalformedAndAmbiguousYaml(final String content) throws IOException {
        Path path = write(content);
        assertThatThrownBy(() -> parser().read(path)).isInstanceOf(IllegalStateException.class).hasMessageNotContaining("super-secret").hasNoCause();
    }

    @Test
    void rejectsMissingFile() {
        assertThatThrownBy(() -> parser().read(directory.resolve("missing.yaml"))).hasMessageContaining("cannot read").hasNoCause();
    }

    @Test
    void rejectsOversizedFile() throws IOException {
        Path path = write("x".repeat(1024 * 1024 + 1));
        assertThatThrownBy(() -> parser().read(path)).hasMessageContaining("exceeds 1 MiB");
    }

    @Test
    void rejectsMissingRuntimeImplementation() throws IOException {
        FileConfigurationParser parser = new FileConfigurationParser(new MockEnvironment(), "namespace", List.of(), Set.of("divide"), Set.of("divide"));
        Path path = write(document().toString());
        assertThatThrownBy(() -> parser.read(path)).hasMessageContaining("missing from the classpath");
    }

    @Test
    void rejectsMissingUpstreamHandler() throws IOException {
        FileConfigurationParser parser = new FileConfigurationParser(new MockEnvironment(), "namespace", plugins(), Set.of("divide", "contextPath"), Set.of());
        Path path = write(document().toString());
        assertThatThrownBy(() -> parser.read(path)).hasMessageContaining("handlers are required");
    }
}
