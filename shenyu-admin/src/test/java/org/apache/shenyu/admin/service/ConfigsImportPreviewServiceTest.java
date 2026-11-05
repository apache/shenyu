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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.mapper.ShenyuDictMapper;
import org.apache.shenyu.admin.model.dto.NamespacePluginDTO;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.NamespacePluginRelDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.result.ConfigsImportPreview;
import org.apache.shenyu.admin.service.configs.plan.ConfigsImportPreviewBusyException;
import org.apache.shenyu.admin.service.configs.plan.ConfigsImportPreviewServiceImpl;
import org.apache.shenyu.admin.utils.ZipUtil;
import org.apache.shenyu.common.utils.JsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Semaphore;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Read-only planning tests for the namespace import preview.
 */
@ExtendWith(MockitoExtension.class)
public final class ConfigsImportPreviewServiceTest {

    private static final String NAMESPACE = "target-namespace";

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private NamespacePluginRelMapper namespacePluginRelMapper;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private RuleMapper ruleMapper;

    @Mock
    private ShenyuDictMapper shenyuDictMapper;

    private ConfigsImportPreviewServiceImpl service;

    @BeforeEach
    public void setUp() {
        service = new ConfigsImportPreviewServiceImpl(pluginMapper, namespacePluginRelMapper, selectorMapper,
                ruleMapper, shenyuDictMapper);
        ReflectionTestUtils.setField(service, "maxEntrySize", 1048576L);
        ReflectionTestUtils.setField(service, "maxTotalSize", 2097152L);
        ReflectionTestUtils.setField(service, "maxEntryCount", 100);
        ReflectionTestUtils.setField(service, "maxOutcomes", 1000);
        ReflectionTestUtils.setField(service, "maxDiagnosticsPerOutcome", 5);
        ReflectionTestUtils.setField(service, "maxRefLength", 128);
        ReflectionTestUtils.setField(service, "maxConcurrent", 1);
        lenient().when(shenyuDictMapper.findByTypeBatch(anyList())).thenReturn(Collections.emptyList());
    }

    @Test
    public void testPreviewPlansCreateReuseSkipAndReject() {
        when(pluginMapper.selectAllIdentity()).thenReturn(List.of(PluginDO.builder().id("p-existing").name("divide").build()));
        when(namespacePluginRelMapper.listByNamespaceId(NAMESPACE)).thenReturn(List.of(
                NamespacePluginRelDO.builder().id("np-existing").pluginId("p-existing").namespaceId(NAMESPACE).build()));
        when(selectorMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(List.of(
                SelectorDO.builder().id("sel-existing").pluginId("p-existing").selectorName("existingSel")
                        .namespaceId(NAMESPACE).build()));
        when(ruleMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(List.of(
                RuleDO.builder().id("rule-existing").selectorId("sel-existing").ruleName("existingRule")
                        .namespaceId(NAMESPACE).build()));

        byte[] archive = ZipUtil.zip(List.of(
                new ZipUtil.ZipItem("plugin_template.json", JsonUtils.toJson(List.of(
                        plugin("src-divide", "divide", "{\"apiKey\":\"TOP_SECRET\"}"),
                        plugin("src-new", "newPlugin", "{\"apiKey\":\"TOP_SECRET\"}")))),
                new ZipUtil.ZipItem("namespace_plugin.json", JsonUtils.toJson(List.of(
                        namespacePlugin("np-1", "src-divide", "divide"),
                        namespacePlugin("np-2", "src-new", "newPlugin"),
                        namespacePlugin("np-3", "missing-src", "missing")))),
                new ZipUtil.ZipItem("selector.json", JsonUtils.toJson(List.of(
                        selector("sel-src-1", "src-divide", "existingSel"),
                        selector("sel-src-2", "src-divide", "newSel", "source-ns"),
                        selector("sel-src-3", "src-divide", "newSel"),
                        selector("sel-src-4", "missing-src", "missingSel")))),
                new ZipUtil.ZipItem("rule.json", JsonUtils.toJson(List.of(
                        rule("rule-src-1", "sel-src-1", "existingRule"),
                        rule("rule-src-2", "sel-src-2", "newRule", "{\"secret\":\"TOP_SECRET\"}"),
                        rule("rule-src-3", "unknown-sel", "orphan"),
                        rule("rule-src-4", "sel-src-1", "newRule"))))));

        ConfigsImportPreview preview = service.preview(NAMESPACE, archive);

        assertThat(preview.fullyValidated()).isFalse();
        assertThat(preview.truncated()).isFalse();
        assertThat(preview.archiveDigest()).startsWith("sha256:");
        assertThat(preview.baselineFingerprint()).startsWith("sha256:");
        assertThat(preview.previewToken()).startsWith("sha256:");
        assertThat(preview.summary()).isEqualTo(new ConfigsImportPreview.Summary(5, 3, 2, 3));
        assertThat(preview.warnings()).anyMatch(value -> value.startsWith("GLOBAL_PLUGIN_TEMPLATE"));
        assertThat(preview.warnings()).anyMatch(value -> value.startsWith("COVERAGE_GAPS_PRESENT"));

        ConfigsImportPreview.SectionPreview pluginSection = section(preview, "plugin_template.json");
        assertThat(pluginSection.scope()).isEqualTo("GLOBAL");
        assertThat(outcome(pluginSection, "src-divide").action()).isEqualTo("REUSE");
        assertThat(outcome(pluginSection, "src-divide").targetRef()).isEqualTo("existing:p-existing");
        assertThat(outcome(pluginSection, "src-new").action()).isEqualTo("CREATE");
        assertThat(outcome(pluginSection, "src-new").diagnostics()).contains("GLOBAL_EFFECT");

        ConfigsImportPreview.SectionPreview namespacePluginSection = section(preview, "namespace_plugin.json");
        assertThat(namespacePluginSection.outcomes()).isNotEmpty();
        assertThat(outcome(namespacePluginSection, "np-1").action()).isEqualTo("SKIP");
        assertThat(outcome(namespacePluginSection, "np-1").diagnostics()).contains("DUPLICATE_NAMESPACE_PLUGIN");
        assertThat(outcome(namespacePluginSection, "np-2").action()).isEqualTo("CREATE");
        assertThat(outcome(namespacePluginSection, "np-3").action()).isEqualTo("REJECT");
        assertThat(outcome(namespacePluginSection, "np-3").diagnostics()).contains("MISSING_PLUGIN_TEMPLATE");

        ConfigsImportPreview.SectionPreview selectorSection = section(preview, "selector.json");
        assertThat(outcome(selectorSection, "sel-src-1").action()).isEqualTo("REUSE");
        assertThat(outcome(selectorSection, "sel-src-1").diagnostics()).contains("DUPLICATE_SELECTOR_NAME");
        assertThat(outcome(selectorSection, "sel-src-2").action()).isEqualTo("CREATE");
        assertThat(outcome(selectorSection, "sel-src-2").diagnostics()).contains("NAMESPACE_REMAPPED");
        assertThat(outcome(selectorSection, "sel-src-3").action()).isEqualTo("REUSE");
        assertThat(outcome(selectorSection, "sel-src-3").targetRef()).isEqualTo("planned");
        assertThat(outcome(selectorSection, "sel-src-4").action()).isEqualTo("REJECT");

        ConfigsImportPreview.SectionPreview ruleSection = section(preview, "rule.json");
        assertThat(outcome(ruleSection, "rule-src-1").action()).isEqualTo("SKIP");
        assertThat(outcome(ruleSection, "rule-src-1").diagnostics()).contains("DUPLICATE_RULE_NAME");
        assertThat(outcome(ruleSection, "rule-src-2").action()).isEqualTo("CREATE");
        assertThat(outcome(ruleSection, "rule-src-3").diagnostics()).contains("MISSING_SELECTOR_REFERENCE");
        assertThat(outcome(ruleSection, "rule-src-4").action()).isEqualTo("CREATE");

        assertThat(JsonUtils.toJson(preview)).doesNotContain("TOP_SECRET");
        verify(pluginMapper, never()).insertSelective(any());
        verify(namespacePluginRelMapper, never()).insertSelective(any());
        verify(selectorMapper, never()).insertSelective(any());
        verify(ruleMapper, never()).insertSelective(any());
    }

    @Test
    public void testUnsupportedSectionsAreNotParsedAndInvalidateCoverage() {
        when(pluginMapper.selectAllIdentity()).thenReturn(Collections.emptyList());
        when(namespacePluginRelMapper.listByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(selectorMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(ruleMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());

        byte[] archive = ZipUtil.zip(List.of(
                new ZipUtil.ZipItem("auth.json", "[{\"appKey\":\"TOP_SECRET\"}]"),
                new ZipUtil.ZipItem("plugin.json", "[{\"name\":\"TOP_SECRET\"}]"),
                new ZipUtil.ZipItem("plugin_handle.json", "[{\"secret\":\"TOP_SECRET\"}]"),
                new ZipUtil.ZipItem("dict.json", "[{\"dictValue\":\"TOP_SECRET\"}]"),
                new ZipUtil.ZipItem("unknown.txt", "TOP_SECRET")));

        ConfigsImportPreview preview = service.preview(NAMESPACE, archive);

        assertThat(preview.fullyValidated()).isFalse();
        assertThat(section(preview, "auth.json").supported()).isFalse();
        assertThat(section(preview, "auth.json").diagnostics()).contains("SECTION_NOT_SUPPORTED");
        assertThat(section(preview, "plugin.json").diagnostics()).contains("UNKNOWN_SECTION");
        assertThat(section(preview, "plugin_handle.json").diagnostics()).contains("SECTION_NOT_SUPPORTED");
        assertThat(section(preview, "dict.json").scope()).isEqualTo("GLOBAL");
        assertThat(section(preview, "dict.json").diagnostics()).contains("SECTION_NOT_SUPPORTED");
        assertThat(section(preview, "unknown.txt").supported()).isFalse();
        assertThat(section(preview, "unknown.txt").diagnostics()).contains("UNKNOWN_SECTION");
        assertThat(JsonUtils.toJson(preview)).doesNotContain("TOP_SECRET");
    }

    @Test
    public void testMalformedZipIsRejected() {
        byte[] archive = "not-a-zip-archive".getBytes(StandardCharsets.UTF_8);

        assertThatThrownBy(() -> service.preview(NAMESPACE, archive))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("malformed zip archive");
    }

    @Test
    public void testZipWithPkPrefixButNoEntriesIsRejected() {
        byte[] archive = {'P', 'K', 0, 0};

        assertThatThrownBy(() -> service.preview(NAMESPACE, archive))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("malformed zip archive");
    }

    @Test
    public void testPreviewTokenDetectsBaselineDrift() {
        when(pluginMapper.selectAllIdentity()).thenReturn(Collections.emptyList());
        when(namespacePluginRelMapper.listByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(selectorMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(ruleMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());

        byte[] archive = ZipUtil.zip(List.of(new ZipUtil.ZipItem("plugin_template.json",
                JsonUtils.toJson(List.of(plugin("src-new", "newPlugin", null))))));
        ConfigsImportPreview preview = service.preview(NAMESPACE, archive);

        assertThat(service.matchesToken(NAMESPACE, archive, preview.previewToken())).isTrue();

        when(selectorMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(List.of(
                SelectorDO.builder().id("drift").pluginId("p").selectorName("drift").build()));
        assertThat(service.matchesToken(NAMESPACE, archive, preview.previewToken())).isFalse();
    }

    @Test
    public void testMissingRequiredFieldsAreRejectedAndBlocking() {
        when(pluginMapper.selectAllIdentity()).thenReturn(Collections.emptyList());
        when(namespacePluginRelMapper.listByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(selectorMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(ruleMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());

        PluginDTO invalidPlugin = new PluginDTO();
        invalidPlugin.setId("bad-plugin");
        invalidPlugin.setName("badPlugin");

        SelectorDTO invalidSelector = new SelectorDTO();
        invalidSelector.setId("bad-selector");
        invalidSelector.setPluginId("bad-plugin");
        invalidSelector.setName("badSelector");

        byte[] archive = ZipUtil.zip(List.of(
                new ZipUtil.ZipItem("plugin_template.json", JsonUtils.toJson(List.of(invalidPlugin))),
                new ZipUtil.ZipItem("selector.json", JsonUtils.toJson(List.of(invalidSelector)))));

        ConfigsImportPreview preview = service.preview(NAMESPACE, archive);

        ConfigsImportPreview.SectionPreview pluginSection = section(preview, "plugin_template.json");
        assertThat(pluginSection.blocking()).isTrue();
        assertThat(outcome(pluginSection, "bad-plugin").action()).isEqualTo("REJECT");
        assertThat(outcome(pluginSection, "bad-plugin").diagnostics()).contains("INVALID_PLUGIN");
        ConfigsImportPreview.SectionPreview selectorSection = section(preview, "selector.json");
        assertThat(selectorSection.blocking()).isTrue();
        assertThat(outcome(selectorSection, "bad-selector").diagnostics()).contains("INVALID_SELECTOR");
        assertThat(preview.fullyValidated()).isFalse();
    }

    @Test
    public void testBlankSupportedSectionIsEmpty() {
        when(pluginMapper.selectAllIdentity()).thenReturn(Collections.emptyList());
        when(namespacePluginRelMapper.listByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(selectorMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(ruleMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        byte[] archive = ZipUtil.zip(List.of(new ZipUtil.ZipItem("selector.json", "")));

        ConfigsImportPreview preview = service.preview(NAMESPACE, archive);

        ConfigsImportPreview.SectionPreview selectorSection = section(preview, "selector.json");
        assertThat(selectorSection.supported()).isTrue();
        assertThat(selectorSection.outcomes()).isEmpty();
        assertThat(selectorSection.diagnostics()).isEmpty();
        assertThat(preview.fullyValidated()).isTrue();
    }

    @Test
    public void testMalformedSupportedSectionIsReported() {
        when(pluginMapper.selectAllIdentity()).thenReturn(Collections.emptyList());
        when(namespacePluginRelMapper.listByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(selectorMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(ruleMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        byte[] archive = ZipUtil.zip(List.of(new ZipUtil.ZipItem("selector.json", "not-json")));

        ConfigsImportPreview preview = service.preview(NAMESPACE, archive);

        ConfigsImportPreview.SectionPreview selectorSection = section(preview, "selector.json");
        assertThat(selectorSection.supported()).isTrue();
        assertThat(selectorSection.blocking()).isTrue();
        assertThat(selectorSection.diagnostics()).contains("SECTION_MALFORMED");
        assertThat(preview.fullyValidated()).isFalse();
    }

    @Test
    public void testConcurrentPreviewBudgetRejectsWhenSaturated() {
        ReflectionTestUtils.setField(service, "previewSemaphore", new Semaphore(0));

        assertThatThrownBy(() -> service.preview(NAMESPACE, ZipUtil.zip(List.of())))
                .isInstanceOf(ConfigsImportPreviewBusyException.class);
    }

    @Test
    public void testZipEntryLimitIsEnforced() {
        ReflectionTestUtils.setField(service, "maxEntryCount", 1);
        byte[] archive = ZipUtil.zip(List.of(
                new ZipUtil.ZipItem("one.json", "[]"),
                new ZipUtil.ZipItem("two.json", "[]")));

        assertThatThrownBy(() -> service.preview(NAMESPACE, archive))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("entry count exceeds maximum");
    }

    @Test
    public void testZipEntrySizeLimitIsEnforced() {
        ReflectionTestUtils.setField(service, "maxEntrySize", 1L);
        byte[] archive = ZipUtil.zip(List.of(new ZipUtil.ZipItem("selector.json", "[]")));

        assertThatThrownBy(() -> service.preview(NAMESPACE, archive))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("entry size exceeds maximum");
    }

    @Test
    public void testZipEntryOrderDoesNotChangeThePlan() {
        when(pluginMapper.selectAllIdentity()).thenReturn(Collections.emptyList());
        when(namespacePluginRelMapper.listByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(selectorMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(ruleMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());

        ZipUtil.ZipItem pluginTemplate = new ZipUtil.ZipItem("plugin_template.json",
                JsonUtils.toJson(List.of(plugin("src-1", "one", null), plugin("src-2", "two", null))));
        ZipUtil.ZipItem unknownFirst = new ZipUtil.ZipItem("a-unknown.txt", "x");
        ZipUtil.ZipItem unknownSecond = new ZipUtil.ZipItem("z-unknown.txt", "x");

        ConfigsImportPreview first = service.preview(NAMESPACE,
                ZipUtil.zip(List.of(pluginTemplate, unknownFirst, unknownSecond)));
        ConfigsImportPreview second = service.preview(NAMESPACE,
                ZipUtil.zip(List.of(unknownSecond, pluginTemplate, unknownFirst)));

        assertThat(second.sections()).isEqualTo(first.sections());
        assertThat(second.summary()).isEqualTo(first.summary());
    }

    @Test
    public void testOutcomeBudgetTruncatesReport() {
        ReflectionTestUtils.setField(service, "maxOutcomes", 1);
        when(pluginMapper.selectAllIdentity()).thenReturn(Collections.emptyList());
        when(namespacePluginRelMapper.listByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(selectorMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(ruleMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());

        byte[] archive = ZipUtil.zip(List.of(new ZipUtil.ZipItem("plugin_template.json",
                JsonUtils.toJson(List.of(plugin("src-1", "one", null), plugin("src-2", "two", null))))));

        ConfigsImportPreview preview = service.preview(NAMESPACE, archive);

        assertThat(preview.truncated()).isTrue();
        assertThat(preview.fullyValidated()).isFalse();
        assertThat(section(preview, "plugin_template.json").outcomes()).hasSize(1);
        assertThat(section(preview, "plugin_template.json").diagnostics()).contains("REPORT_TRUNCATED");
        assertThat(preview.warnings()).anyMatch(value -> value.startsWith("REPORT_TRUNCATED"));
    }

    @Test
    public void testBlockingRuleValidationMarksSection() {
        when(pluginMapper.selectAllIdentity()).thenReturn(List.of(
                PluginDO.builder().id("p-agent").name("agentGateway").build()));
        when(namespacePluginRelMapper.listByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());
        when(selectorMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(List.of(
                SelectorDO.builder().id("sel-agent").pluginId("p-agent").selectorName("agentSel")
                        .namespaceId(NAMESPACE).build()));
        when(ruleMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.emptyList());

        byte[] archive = ZipUtil.zip(List.of(
                new ZipUtil.ZipItem("plugin_template.json", JsonUtils.toJson(List.of(
                        plugin("src-agent", "agentGateway", "{}")))),
                new ZipUtil.ZipItem("selector.json", JsonUtils.toJson(List.of(
                        selector("sel-src", "src-agent", "agentSel")))),
                new ZipUtil.ZipItem("rule.json", JsonUtils.toJson(List.of(
                        rule("rule-bad", "sel-src", "badRule", "not-json"))))));

        ConfigsImportPreview preview = service.preview(NAMESPACE, archive);

        ConfigsImportPreview.SectionPreview ruleSection = section(preview, "rule.json");
        assertThat(ruleSection.blocking()).isTrue();
        assertThat(outcome(ruleSection, "rule-bad").action()).isEqualTo("REJECT");
        assertThat(outcome(ruleSection, "rule-bad").diagnostics()).contains("INVALID_RULE_HANDLE");
        assertThat(preview.warnings()).anyMatch(value -> value.startsWith("IMPORT_WOULD_FAIL_FAST"));
    }

    private static ConfigsImportPreview.SectionPreview section(final ConfigsImportPreview preview, final String name) {
        return preview.sections().stream().filter(section -> name.equals(section.section()))
                .findFirst().orElseThrow();
    }

    private static ConfigsImportPreview.PreviewOutcome outcome(final ConfigsImportPreview.SectionPreview section,
                                                              final String sourceId) {
        return section.outcomes().stream().filter(outcome -> outcome.sourceRef().endsWith("#" + sourceId))
                .findFirst().orElseThrow();
    }

    private static PluginDTO plugin(final String id, final String name, final String config) {
        PluginDTO pluginDTO = new PluginDTO();
        pluginDTO.setId(id);
        pluginDTO.setName(name);
        pluginDTO.setConfig(config);
        pluginDTO.setRole("Proxy");
        pluginDTO.setSort(0);
        pluginDTO.setEnabled(false);
        return pluginDTO;
    }

    private static NamespacePluginDTO namespacePlugin(final String id, final String pluginId, final String name) {
        NamespacePluginDTO pluginDTO = new NamespacePluginDTO();
        pluginDTO.setId(id);
        pluginDTO.setPluginId(pluginId);
        pluginDTO.setName(name);
        pluginDTO.setEnabled(false);
        pluginDTO.setSort(0);
        return pluginDTO;
    }

    private static SelectorDTO selector(final String id, final String pluginId, final String name) {
        return selector(id, pluginId, name, null);
    }

    private static SelectorDTO selector(final String id, final String pluginId, final String name,
                                        final String namespaceId) {
        return SelectorDTO.builder().id(id).pluginId(pluginId).name(name).namespaceId(namespaceId)
                .type(0).sort(0).enabled(true).logged(false).continued(true).matchRestful(false)
                .selectorConditions(Collections.emptyList()).build();
    }

    private static RuleDTO rule(final String id, final String selectorId, final String name) {
        return rule(id, selectorId, name, "{}");
    }

    private static RuleDTO rule(final String id, final String selectorId, final String name, final String handle) {
        return RuleDTO.builder().id(id).selectorId(selectorId).name(name).handle(handle)
                .matchMode(0).sort(0).enabled(true).logged(false).matchRestful(false)
                .ruleConditions(Collections.emptyList()).build();
    }
}
