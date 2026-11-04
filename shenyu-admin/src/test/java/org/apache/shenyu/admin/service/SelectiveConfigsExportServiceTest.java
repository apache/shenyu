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

import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.SelectorExportDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.model.vo.PluginHandleVO;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.model.vo.RuleConditionVO;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.model.vo.SelectorConditionVO;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.utils.ZipUtil;
import org.apache.shenyu.common.constant.ExportImportConstants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shiro.authz.UnauthorizedException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Verify selective export boundaries before producing any ZIP.
 */
@ExtendWith(MockitoExtension.class)
public class SelectiveConfigsExportServiceTest {

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private RuleMapper ruleMapper;

    @Mock
    private SelectorService selectorService;

    @Mock
    private RuleService ruleService;

    @Mock
    private PluginService pluginService;

    @Mock
    private NamespacePluginService namespacePluginService;

    @Mock
    private PluginHandleService pluginHandleService;

    @Mock
    private DiscoveryMapper discoveryMapper;

    @Mock
    private DiscoveryHandlerMapper handlerMapper;

    @Mock
    private DiscoveryRelMapper relMapper;

    @Mock
    private DiscoveryUpstreamMapper upstreamMapper;

    @InjectMocks
    private SelectiveConfigsExportService service;

    @Test
    public void testInlineHandleWithoutLocalDiscoveryIsRejected() {
        selector();
        PluginVO template = new PluginVO();
        template.setId("divide");
        template.setName("divide");
        when(pluginService.findById("divide")).thenReturn(template);
        assertTrue(assertThrows(ShenyuException.class, () -> service.export(request(List.of("orders")))).getMessage().contains("inline-only"));
        verifyNoInteractions(discoveryMapper, relMapper, upstreamMapper, ruleMapper, namespacePluginService);
    }

    @Test
    public void testAllNamespaceMembershipIsCheckedBeforeConfigurationReads() {
        when(selectorMapper.selectNamespaceIdById("orders")).thenReturn("source");
        when(selectorMapper.selectNamespaceIdById("foreign")).thenReturn("other");
        assertThrows(ShenyuException.class, () -> service.export(request(List.of("orders", "foreign"))));
        verifyNoInteractions(selectorService, pluginService, ruleService, handlerMapper);
    }

    @Test
    public void testDeniedSelectorDoesNotReadDependencies() {
        when(selectorMapper.selectNamespaceIdById("orders")).thenReturn("source");
        when(selectorService.findById("orders")).thenThrow(new UnauthorizedException("denied"));
        assertThrows(UnauthorizedException.class, () -> service.export(request(List.of("orders"))));
        verifyNoInteractions(pluginService, ruleService, handlerMapper);
    }

    @Test
    public void testUnsupportedPluginIsRejected() {
        selector();
        PluginVO plugin = new PluginVO();
        plugin.setName("dubbo");
        when(pluginService.findById("divide")).thenReturn(plugin);
        assertTrue(assertThrows(ShenyuException.class, () -> service.export(request(List.of("orders")))).getMessage().contains("Only divide"));
        verifyNoInteractions(handlerMapper, ruleMapper);
    }

    @Test
    public void testInvalidSelectionsFailBeforeQueries() {
        assertThrows(ShenyuException.class, () -> service.export(request(Collections.emptyList())));
        assertThrows(ShenyuException.class, () -> service.export(request(Collections.nCopies(101, "orders"))));
        assertThrows(ShenyuException.class, () -> service.export(request(List.of("orders", "orders"))));
        assertThrows(ShenyuException.class, () -> service.export(request(List.of(" "))));
        verifyNoInteractions(selectorMapper, selectorService);
    }

    @Test
    public void testExactlyOneThousandObjectsProduceReadableArchive() {
        // 989 rule conditions + one selector condition + ten other dependency objects.
        quotaSelection(989, 1);
        List<ZipUtil.ZipItem> sections = sections(service.export(request(List.of("orders"))));
        assertEquals(8, sections.size());
        assertTrue(sections.stream().anyMatch(item -> ExportImportConstants.DISCOVERY_JSON.equals(item.getItemName())));
        assertTrue(sections.stream().anyMatch(item -> ExportImportConstants.PLUGIN_HANDLE_JSON.equals(item.getItemName())));
        assertTrue(sections.stream().anyMatch(item -> ExportImportConstants.DICT_JSON.equals(item.getItemName())));
        assertTrue(sections.stream().filter(item -> ExportImportConstants.RULE_JSON.equals(item.getItemName()))
                .anyMatch(item -> item.getItemData().contains("condition-988")));
    }

    @Test
    public void testOneAdditionalConditionExceedsObjectBudget() {
        quotaSelection(990, 1);
        assertEquals("Selection exceeds maximum 1000 configuration objects",
                assertThrows(ShenyuException.class, () -> service.export(request(List.of("orders")))).getMessage());
    }

    @Test
    public void testDictionaryOptionsAreIncludedInObjectBudget() {
        quotaSelection(989, 2);
        assertEquals("Selection exceeds maximum 1000 configuration objects",
                assertThrows(ShenyuException.class, () -> service.export(request(List.of("orders")))).getMessage());
    }

    @Test
    public void testUncompressedUtf8BytesAtAndAboveTenMiBBudget() {
        NamespacePluginVO namespacePlugin = quotaSelection(1, 1);
        namespacePlugin.setConfig("{\"note\":\"中\"}");
        List<ZipUtil.ZipItem> baseline = sections(service.export(request(List.of("orders"))));
        long baselineBytes = utf8Size(baseline);
        long characterCount = baseline.stream().mapToLong(item -> item.getItemData().length()).sum();
        assertTrue(baselineBytes > characterCount, "The fixture must contain a multibyte UTF-8 character");
        int budget = 10 * 1024 * 1024;
        String padding = "a".repeat(Math.toIntExact(budget - baselineBytes));
        namespacePlugin.setConfig("{\"note\":\"中" + padding + "\"}");
        byte[] archive = service.export(request(List.of("orders")));
        assertEquals(budget, utf8Size(sections(archive)));
        assertTrue(archive.length < budget, "The byte budget applies before ZIP compression");
        namespacePlugin.setConfig("{\"note\":\"中" + padding + "a\"}");
        assertEquals("Selection exceeds maximum 10 MiB uncompressed export size",
                assertThrows(ShenyuException.class, () -> service.export(request(List.of("orders")))).getMessage());
    }

    private NamespacePluginVO quotaSelection(final int conditionCount, final int optionCount) {
        when(selectorMapper.selectNamespaceIdById("orders")).thenReturn("source");
        when(selectorService.findById("orders")).thenAnswer(invocation -> {
            SelectorVO value = new SelectorVO();
            value.setId("orders");
            value.setName("orders");
            value.setPluginId("divide");
            value.setNamespaceId("source");
            value.setHandle("[]");
            SelectorConditionVO condition = new SelectorConditionVO();
            condition.setId("selector-condition");
            condition.setSelectorId("orders");
            condition.setParamType("uri");
            condition.setOperator("match");
            condition.setParamName("/");
            condition.setParamValue("/orders/**");
            value.setSelectorConditions(List.of(condition));
            return value;
        });
        PluginVO template = new PluginVO();
        template.setId("divide");
        template.setName("divide");
        when(pluginService.findById("divide")).thenReturn(template);
        localDiscovery();
        quotaRule(conditionCount);
        NamespacePluginVO namespacePlugin = new NamespacePluginVO();
        namespacePlugin.setId("namespace-plugin");
        namespacePlugin.setPluginId("divide");
        namespacePlugin.setNamespaceId("source");
        namespacePlugin.setConfig("{}");
        when(namespacePluginService.findByNamespaceIdAndPluginId("source", "divide")).thenReturn(namespacePlugin);
        quotaSchema(optionCount);
        return namespacePlugin;
    }

    private void localDiscovery() {
        DiscoveryDO discovery = new DiscoveryDO();
        discovery.setId("discovery");
        discovery.setDiscoveryName("orders");
        discovery.setNamespaceId("source");
        discovery.setPluginName("divide");
        discovery.setDiscoveryType("local");
        discovery.setDiscoveryLevel("0");
        when(discoveryMapper.selectById("discovery")).thenReturn(discovery);
        DiscoveryHandlerDO handler = new DiscoveryHandlerDO();
        handler.setId("handler");
        handler.setDiscoveryId("discovery");
        handler.setHandler("{}");
        handler.setProps("{}");
        when(handlerMapper.selectBySelectorId("orders")).thenReturn(handler);
        when(handlerMapper.selectByDiscoveryId("discovery")).thenReturn(List.of(handler));
        DiscoveryRelDO relation = new DiscoveryRelDO();
        relation.setId("relation");
        relation.setSelectorId("orders");
        relation.setPluginName("divide");
        relation.setDiscoveryHandlerId("handler");
        when(relMapper.selectByDiscoveryHandlerIds(List.of("handler"))).thenReturn(List.of(relation));
        DiscoveryUpstreamDO upstream = new DiscoveryUpstreamDO();
        upstream.setId("upstream");
        upstream.setDiscoveryHandlerId("handler");
        upstream.setNamespaceId("source");
        upstream.setProtocol("http");
        upstream.setUpstreamUrl("127.0.0.1:8080");
        upstream.setWeight(50);
        when(upstreamMapper.selectByDiscoveryHandlerId("handler")).thenReturn(List.of(upstream));
    }

    private void quotaRule(final int conditionCount) {
        RuleDO row = new RuleDO();
        row.setId("rule");
        row.setSelectorId("orders");
        row.setNamespaceId("source");
        when(ruleMapper.countByQuery(any(RuleQuery.class))).thenReturn(1);
        when(ruleMapper.findBySelectorId("orders")).thenReturn(List.of(row));
        RuleVO rule = new RuleVO();
        rule.setId("rule");
        rule.setSelectorId("orders");
        rule.setName("orders");
        rule.setHandle("{\"loadBalance\":\"random\"}");
        rule.setRuleConditions(IntStream.range(0, conditionCount).mapToObj(index -> {
            RuleConditionVO condition = new RuleConditionVO();
            condition.setId("condition-" + index);
            condition.setRuleId("rule");
            condition.setParamType("uri");
            condition.setOperator("match");
            condition.setParamName("/");
            condition.setParamValue("/orders/**");
            return condition;
        }).collect(Collectors.toList()));
        when(ruleService.findById("rule")).thenReturn(rule);
    }

    private void quotaSchema(final int optionCount) {
        PluginHandleVO handle = new PluginHandleVO();
        handle.setId("schema");
        handle.setPluginId("divide");
        handle.setField("mode");
        handle.setLabel("Mode");
        handle.setDataType(3);
        handle.setType(2);
        handle.setDictOptions(IntStream.range(0, optionCount).mapToObj(index -> {
            ShenyuDictVO option = new ShenyuDictVO();
            option.setId("option-" + index);
            option.setType("mode");
            option.setDictCode("mode");
            option.setDictName("mode-" + index);
            option.setDictValue("value-" + index);
            option.setEnabled(true);
            return option;
        }).collect(Collectors.toList()));
        when(pluginHandleService.listAllDataByPluginIds(List.of("divide"))).thenReturn(List.of(handle));
    }

    private List<ZipUtil.ZipItem> sections(final byte[] archive) {
        return ZipUtil.unzip(archive, 10 * 1024 * 1024, 10 * 1024 * 1024, 8).getZipItemList();
    }

    private long utf8Size(final List<ZipUtil.ZipItem> items) {
        return items.stream().mapToLong(item -> item.getItemData().getBytes(StandardCharsets.UTF_8).length).sum();
    }

    private void selector() {
        when(selectorMapper.selectNamespaceIdById("orders")).thenReturn("source");
        SelectorVO selector = new SelectorVO();
        selector.setId("orders");
        selector.setName("orders");
        selector.setPluginId("divide");
        selector.setNamespaceId("source");
        selector.setHandle("[{\"protocol\":\"http\",\"upstreamUrl\":\"127.0.0.1:8080\",\"weight\":50}]");
        when(selectorService.findById("orders")).thenReturn(selector);
    }

    private SelectorExportDTO request(final List<String> ids) {
        SelectorExportDTO request = new SelectorExportDTO();
        request.setNamespace("source");
        request.setSelectorIds(ids);
        return request;
    }
}
