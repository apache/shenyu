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

import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.model.dto.NamespacePluginDTO;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.enums.ConfigImportAction;
import org.apache.shenyu.admin.model.result.ConfigsImportPreview;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.configs.plan.ConfigsImportPreviewService;
import org.apache.shenyu.admin.utils.ZipUtil;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.ExportImportConstants;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.JsonUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.event.ApplicationEvents;
import org.springframework.test.context.event.RecordApplicationEvents;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * H2 integration coverage for the read-only import preview.
 */
@RecordApplicationEvents
public class ConfigsImportPreviewIntegrationTest extends AbstractSpringIntegrationTest {

    private static final String NAMESPACE = Constants.SYS_DEFAULT_NAMESPACE_ID;

    private static final String SELECTOR_NAME = "preview-test-selector";

    private static final String RULE_NAME = "preview-test-rule";

    @Resource
    private ConfigsImportPreviewService previewService;

    @Resource
    private ConfigsService configsService;

    @Resource
    private JdbcTemplate jdbcTemplate;

    @Resource
    private ApplicationEvents applicationEvents;

    @SpyBean
    private SyncDataService syncDataService;

    @BeforeEach
    public void setUp() {
        cleanup();
    }

    @AfterEach
    public void tearDown() {
        cleanup();
    }

    private void cleanup() {
        jdbcTemplate.update("DELETE FROM rule_condition WHERE rule_id IN (SELECT id FROM `rule` WHERE rule_name = ?)", RULE_NAME);
        jdbcTemplate.update("DELETE FROM selector_condition WHERE selector_id IN (SELECT id FROM selector WHERE selector_name = ?)", SELECTOR_NAME);
        jdbcTemplate.update("DELETE FROM `rule` WHERE rule_name = ?", RULE_NAME);
        jdbcTemplate.update("DELETE FROM selector WHERE selector_name = ?", SELECTOR_NAME);
        jdbcTemplate.update("DELETE FROM operation_record_log WHERE context LIKE ?", "%" + SELECTOR_NAME + "%");
    }

    @Test
    public void testPreviewIsReadOnlyAndImportMatchesThePlan() {
        byte[] archive = archive();
        final int pluginCount = count("plugin");
        final int namespacePluginCount = count("namespace_plugin_rel");
        final int selectorCount = selectorCount();
        final int ruleCount = ruleCount();
        final int auditCount = count("operation_record_log");
        clearInvocations(syncDataService);

        ConfigsImportPreview preview = previewService.preview(NAMESPACE, archive);

        assertThat(applicationEvents.stream(DataChangedEvent.class)).isEmpty();
        verify(syncDataService, never()).syncAll(any());
        verify(syncDataService, never()).syncAllByNamespaceId(any(), any());
        assertThat(count("plugin")).isEqualTo(pluginCount);
        assertThat(count("namespace_plugin_rel")).isEqualTo(namespacePluginCount);
        assertThat(selectorCount()).isEqualTo(selectorCount);
        assertThat(ruleCount()).isEqualTo(ruleCount);
        assertThat(count("operation_record_log")).isEqualTo(auditCount);
        assertThat(preview.fullyValidated()).isTrue();
        assertThat(preview.summary()).isEqualTo(new ConfigsImportPreview.Summary(2, 1, 1, 0));
        assertThat(outcome(preview, "plugin_template.json", "sign").action()).isEqualTo(ConfigImportAction.REUSE);
        assertThat(outcome(preview, "namespace_plugin.json", "sign").action()).isEqualTo(ConfigImportAction.SKIP);
        assertThat(outcome(preview, "selector.json", SELECTOR_NAME).action()).isEqualTo(ConfigImportAction.CREATE);
        assertThat(outcome(preview, "rule.json", RULE_NAME).action()).isEqualTo(ConfigImportAction.CREATE);

        ShenyuAdminResult result = configsService.configsImport(NAMESPACE, archive);

        assertThat(result.getCode()).isEqualTo(CommonErrorCode.SUCCESSFUL);
        Map<?, ?> data = (Map<?, ?>) result.getData();
        assertThat(data.get(ExportImportConstants.SELECTOR_IMPORT_SUCCESS_COUNT)).isEqualTo(1);
        assertThat(data.get(ExportImportConstants.RULE_IMPORT_SUCCESS_COUNT)).isEqualTo(1);
        assertThat(count("plugin")).isEqualTo(pluginCount);
        assertThat(count("namespace_plugin_rel")).isEqualTo(namespacePluginCount);
        assertThat(selectorCount()).isEqualTo(selectorCount + 1);
        assertThat(ruleCount()).isEqualTo(ruleCount + 1);
    }

    private static ConfigsImportPreview.PreviewOutcome outcome(final ConfigsImportPreview preview,
                                                              final String sectionName,
                                                              final String sourceName) {
        return preview.sections().stream()
                .filter(section -> sectionName.equals(section.section()))
                .flatMap(section -> section.outcomes().stream())
                .filter(outcome -> sourceName.equals(outcome.sourceName()))
                .findFirst()
                .orElseThrow();
    }

    private int count(final String table) {
        return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM " + ("rule".equals(table) ? "`rule`" : table), Integer.class);
    }

    private int selectorCount() {
        return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM selector WHERE namespace_id = ?", Integer.class, NAMESPACE);
    }

    private int ruleCount() {
        return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM `rule` WHERE namespace_id = ?", Integer.class, NAMESPACE);
    }

    private static byte[] archive() {
        PluginDTO plugin = new PluginDTO();
        plugin.setId("preview-test-sign-src");
        plugin.setName("sign");
        plugin.setRole("Authentication");
        plugin.setSort(0);
        plugin.setEnabled(false);

        NamespacePluginDTO namespacePlugin = new NamespacePluginDTO();
        namespacePlugin.setId("preview-test-namespace-plugin-src");
        namespacePlugin.setPluginId("preview-test-sign-src");
        namespacePlugin.setName("sign");
        namespacePlugin.setEnabled(false);
        namespacePlugin.setSort(0);

        SelectorDTO selector = new SelectorDTO();
        selector.setId("preview-test-selector-src");
        selector.setPluginId("preview-test-sign-src");
        selector.setName(SELECTOR_NAME);
        selector.setType(0);
        selector.setSort(0);
        selector.setEnabled(true);
        selector.setLogged(false);
        selector.setContinued(true);
        selector.setMatchRestful(false);
        selector.setSelectorConditions(Collections.emptyList());

        RuleDTO rule = new RuleDTO();
        rule.setId("preview-test-rule-src");
        rule.setSelectorId("preview-test-selector-src");
        rule.setName(RULE_NAME);
        rule.setMatchMode(0);
        rule.setSort(0);
        rule.setEnabled(true);
        rule.setLogged(false);
        rule.setMatchRestful(false);
        rule.setHandle("{}");
        rule.setRuleConditions(Collections.emptyList());

        return ZipUtil.zip(List.of(
                new ZipUtil.ZipItem("plugin_template.json", JsonUtils.toJson(List.of(plugin))),
                new ZipUtil.ZipItem("namespace_plugin.json", JsonUtils.toJson(List.of(namespacePlugin))),
                new ZipUtil.ZipItem("selector.json", JsonUtils.toJson(List.of(selector))),
                new ZipUtil.ZipItem("rule.json", JsonUtils.toJson(List.of(rule)))));
    }
}
