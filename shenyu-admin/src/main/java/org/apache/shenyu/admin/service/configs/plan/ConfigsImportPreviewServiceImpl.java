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

package org.apache.shenyu.admin.service.configs.plan;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
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
import org.apache.shenyu.admin.service.configs.ConfigsExportImportEnum;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.admin.utils.PreviewTextSanitizer;
import org.apache.shenyu.admin.utils.ZipUtil;
import org.apache.shenyu.admin.validation.validator.AgentGatewayPluginConfigValidator;
import org.apache.shenyu.common.constant.ExportImportConstants;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.Semaphore;
import java.util.stream.Collectors;

/**
 * Read-only import preview implementation.
 */
@Service
public class ConfigsImportPreviewServiceImpl implements ConfigsImportPreviewService {

    private static final Logger LOG = LoggerFactory.getLogger(ConfigsImportPreviewServiceImpl.class);

    private static final String SCOPE_NAMESPACE = "NAMESPACE";

    private static final String SCOPE_GLOBAL = "GLOBAL";

    private static final String TARGET_NEW = "new";

    private static final String TARGET_NONE = "none";

    private static final String TARGET_PLANNED = "planned";

    private static final String TARGET_EXISTING = "existing:";

    private static final String INVALID_SOURCE = "INVALID_SOURCE";

    private static final String MISSING_PLUGIN_TEMPLATE = "MISSING_PLUGIN_TEMPLATE";

    private static final String MISSING_SELECTOR_REFERENCE = "MISSING_SELECTOR_REFERENCE";

    private static final String DUPLICATE_SELECTOR_NAME = "DUPLICATE_SELECTOR_NAME";

    private static final String DUPLICATE_NAMESPACE_PLUGIN = "DUPLICATE_NAMESPACE_PLUGIN";

    private static final String DUPLICATE_RULE_NAME = "DUPLICATE_RULE_NAME";

    private static final String INVALID_PLUGIN = "INVALID_PLUGIN";

    private static final String INVALID_NAMESPACE_PLUGIN = "INVALID_NAMESPACE_PLUGIN";

    private static final String INVALID_SELECTOR = "INVALID_SELECTOR";

    private static final String INVALID_RULE = "INVALID_RULE";

    private static final String INVALID_PLUGIN_CONFIG = "INVALID_PLUGIN_CONFIG";

    private static final String INVALID_RULE_HANDLE = "INVALID_RULE_HANDLE";

    private static final String INVALID_CANARY_CONFIG = "INVALID_CANARY_CONFIG";

    private static final String NAMESPACE_REMAPPED = "NAMESPACE_REMAPPED";

    private static final String EMBEDDED_RULES_IGNORED = "EMBEDDED_RULES_IGNORED";

    private static final String GLOBAL_EFFECT = "GLOBAL_EFFECT";

    private static final String SECTION_NOT_SUPPORTED = "SECTION_NOT_SUPPORTED";

    private static final String UNKNOWN_SECTION = "UNKNOWN_SECTION";

    private static final String SECTION_MALFORMED = "SECTION_MALFORMED";

    private static final String REPORT_TRUNCATED = "REPORT_TRUNCATED";

    private static final List<String> VALIDATORS_PLUGIN = List.of("AgentGatewayPluginConfigValidator");

    private static final List<String> VALIDATORS_SELECTOR = List.of("identity-resolution");

    private static final List<String> VALIDATORS_RULE = List.of("identity-resolution",
            "AgentGatewayRuleHandleValidator", "RewriteRuleHandleValidator", "CanaryConfigValidator");

    private static final List<ConfigsExportImportEnum> SECTIONS = Arrays.stream(ConfigsExportImportEnum.values())
            .sorted(Comparator.comparingInt(ConfigsExportImportEnum::getImportOrder)).toList();

    private static final Set<String> KNOWN_SECTION_NAMES = SECTIONS.stream()
            .map(ConfigsExportImportEnum::getConfigName).collect(Collectors.toUnmodifiableSet());

    private final PluginMapper pluginMapper;

    private final NamespacePluginRelMapper namespacePluginRelMapper;

    private final SelectorMapper selectorMapper;

    private final RuleMapper ruleMapper;

    private final ShenyuDictMapper shenyuDictMapper;

    @Value("${shenyu.config.import.max-entry-size:104857600}")
    private long maxEntrySize;

    @Value("${shenyu.config.import.max-total-size:209715200}")
    private long maxTotalSize;

    @Value("${shenyu.config.import.max-entry-count:1000}")
    private int maxEntryCount;

    @Value("${shenyu.config.import.preview.max-outcomes:1000}")
    private int maxOutcomes;

    @Value("${shenyu.config.import.preview.max-diagnostics-per-outcome:5}")
    private int maxDiagnosticsPerOutcome;

    @Value("${shenyu.config.import.preview.max-ref-length:128}")
    private int maxRefLength;

    @Value("${shenyu.config.import.preview.max-concurrent:1}")
    private int maxConcurrent;

    private volatile Semaphore previewSemaphore;

    public ConfigsImportPreviewServiceImpl(final PluginMapper pluginMapper,
                                           final NamespacePluginRelMapper namespacePluginRelMapper,
                                           final SelectorMapper selectorMapper,
                                           final RuleMapper ruleMapper,
                                           final ShenyuDictMapper shenyuDictMapper) {
        this.pluginMapper = pluginMapper;
        this.namespacePluginRelMapper = namespacePluginRelMapper;
        this.selectorMapper = selectorMapper;
        this.ruleMapper = ruleMapper;
        this.shenyuDictMapper = shenyuDictMapper;
    }

    @Override
    @Transactional(readOnly = true)
    public ConfigsImportPreview preview(final String namespace, final byte[] source) {
        if (Objects.isNull(source)) {
            throw new IllegalArgumentException("archive is required");
        }
        final Semaphore semaphore = semaphore();
        if (!semaphore.tryAcquire()) {
            throw new ConfigsImportPreviewBusyException();
        }
        final long started = System.currentTimeMillis();
        try {
            final String observedAt = DateTimeFormatter.ISO_INSTANT.format(Instant.now().truncatedTo(ChronoUnit.SECONDS));
            final ZipUtil.UnZipResult unzipResult = ZipUtil.unzipStrict(source, maxEntrySize, maxTotalSize, maxEntryCount);
            final List<ZipUtil.ZipItem> items = unzipResult.getZipItemList();
            final ConfigsImportTargetSnapshot snapshot = ConfigsImportTargetSnapshot.load(pluginMapper, namespacePluginRelMapper,
                    selectorMapper, ruleMapper, shenyuDictMapper, namespace);
            final String archiveDigest = ConfigsImportFingerprint.archiveDigest(source);
            final String baselineFingerprint = ConfigsImportFingerprint.baseline(namespace, snapshot);
            final String previewToken = ConfigsImportFingerprint.token(namespace, archiveDigest, baselineFingerprint);
            final PreviewPlanning planning = planSections(items, namespace, snapshot, new ConfigsImportContext());
            final ConfigsImportPreview.Summary summary = summarize(planning.sections());
            final List<String> warnings = warnings(planning.sections(), planning.blocking(), planning.truncated());
            LOG.info("configs import preview namespace={} digest={} sections={} truncated={} elapsedMs={}",
                    namespace, archiveDigest, planning.sections().size(), planning.truncated(),
                    System.currentTimeMillis() - started);
            return new ConfigsImportPreview(observedAt, archiveDigest, baselineFingerprint, previewToken, namespace,
                    planning.coverage() && !planning.truncated(), planning.truncated(), summary,
                    planning.sections(), warnings);
        } finally {
            semaphore.release();
        }
    }

    private PreviewPlanning planSections(final List<ZipUtil.ZipItem> items,
                                         final String namespace,
                                         final ConfigsImportTargetSnapshot snapshot,
                                         final ConfigsImportContext context) {
        Map<String, ZipUtil.ZipItem> itemsByName = new LinkedHashMap<>();
        for (ZipUtil.ZipItem item : items) {
            itemsByName.put(item.getItemName(), item);
        }
        PreviewBudget budget = new PreviewBudget(maxOutcomes);
        List<ConfigsImportPreview.SectionPreview> sections = new ArrayList<>();
        boolean coverage = true;
        boolean blocking = false;
        boolean truncated = false;

        Map<String, PluginDO> pluginsById = new LinkedHashMap<>();
        for (PluginDO plugin : snapshot.plugins()) {
            pluginsById.putIfAbsent(plugin.getId(), plugin);
        }
        Map<String, String> selectorPluginIds = new LinkedHashMap<>();
        for (SelectorDO selector : snapshot.selectors()) {
            selectorPluginIds.putIfAbsent(selector.getId(), selector.getPluginId());
        }
        Map<String, String> plannedPluginNamesById = new LinkedHashMap<>();
        Map<String, String> plannedSelectorPluginIds = new LinkedHashMap<>();
        PlanningState state = new PlanningState(namespace, snapshot, context, budget, pluginsById, selectorPluginIds,
                plannedPluginNamesById, plannedSelectorPluginIds);

        for (ConfigsExportImportEnum section : SECTIONS) {
            ZipUtil.ZipItem item = itemsByName.get(section.getConfigName());
            boolean supported = isSupported(section);
            if (Objects.isNull(item)) {
                sections.add(new ConfigsImportPreview.SectionPreview(section.getConfigName(), scopeOf(section),
                        supported, false, false, List.of(), List.of(), List.of()));
                continue;
            }
            if (!supported) {
                sections.add(new ConfigsImportPreview.SectionPreview(section.getConfigName(), scopeOf(section),
                        false, true, false, List.of(), List.of(), List.of(SECTION_NOT_SUPPORTED)));
                coverage = false;
                continue;
            }
            if (StringUtils.isBlank(item.getItemData())) {
                sections.add(new ConfigsImportPreview.SectionPreview(section.getConfigName(), scopeOf(section),
                        true, true, false, List.of(), List.of(), List.of()));
                continue;
            }
            if (budget.isExhausted()) {
                sections.add(new ConfigsImportPreview.SectionPreview(section.getConfigName(), scopeOf(section),
                        true, true, false, List.of(), List.of(), List.of(REPORT_TRUNCATED)));
                coverage = false;
                truncated = true;
                continue;
            }
            SectionPlan plan = planSection(section, item.getItemData(), state);
            sections.add(plan.section());
            coverage = coverage && plan.parsed() && !hasCoverageGap(plan.section());
            blocking = blocking || plan.section().blocking();
            if (plan.section().diagnostics().contains(REPORT_TRUNCATED)) {
                coverage = false;
                truncated = true;
            }
        }

        List<ConfigsImportPreview.SectionPreview> unknownSections = new ArrayList<>();
        for (ZipUtil.ZipItem item : items) {
            if (!KNOWN_SECTION_NAMES.contains(item.getItemName())) {
                unknownSections.add(new ConfigsImportPreview.SectionPreview(
                        PreviewTextSanitizer.sanitize(item.getItemName(), refLimit()), SCOPE_NAMESPACE,
                        false, true, false, List.of(), List.of(), List.of(UNKNOWN_SECTION)));
                coverage = false;
            }
        }
        unknownSections.sort(Comparator.comparing(ConfigsImportPreview.SectionPreview::section));
        sections.addAll(unknownSections);
        return new PreviewPlanning(sections, coverage, blocking, truncated);
    }

    @Override
    @Transactional(readOnly = true)
    public boolean matchesToken(final String namespace, final byte[] source, final String previewToken) {
        if (Objects.isNull(source) || StringUtils.isBlank(previewToken) || previewToken.length() > 256) {
            return false;
        }
        ConfigsImportTargetSnapshot snapshot = ConfigsImportTargetSnapshot.load(pluginMapper, namespacePluginRelMapper,
                selectorMapper, ruleMapper, shenyuDictMapper, namespace);
        String expected = ConfigsImportFingerprint.token(namespace,
                ConfigsImportFingerprint.archiveDigest(source),
                ConfigsImportFingerprint.baseline(namespace, snapshot));
        return Objects.equals(expected, previewToken);
    }

    private SectionPlan planSection(final ConfigsExportImportEnum section,
                                    final String data,
                                    final PlanningState state) {
        return switch (section) {
            case PluginTemplate -> planPluginTemplates(data, state);
            case NamespacePlugin -> planNamespacePlugins(data, state);
            case Selector -> planSelectors(data, state);
            case Rule -> planRules(data, state);
            default -> new SectionPlan(unsupportedSection(section.getConfigName(), scopeOf(section), SECTION_NOT_SUPPORTED), false);
        };
    }

    private SectionPlan planPluginTemplates(final String data, final PlanningState state) {
        final List<PluginDTO> pluginList;
        try {
            pluginList = parse(data, PluginDTO.class);
        } catch (RuntimeException ex) {
            return malformedSection(ExportImportConstants.PLUGIN_TEMPLATE_JSON, SCOPE_GLOBAL, VALIDATORS_PLUGIN);
        }
        Map<String, String> existing = new LinkedHashMap<>();
        for (PluginDO plugin : state.snapshot().plugins()) {
            existing.putIfAbsent(plugin.getName(), plugin.getId());
        }
        PluginTemplateImportResolver resolver = new PluginTemplateImportResolver(existing, state.context().getPluginTemplateIdMapping());
        List<ConfigImportDecision> decisions = new ArrayList<>();
        boolean blocking = false;
        boolean truncated = false;
        for (PluginDTO pluginDTO : pluginList) {
            if (!state.budget().tryConsume()) {
                truncated = true;
                break;
            }
            if (Objects.isNull(pluginDTO) || StringUtils.isBlank(pluginDTO.getName())) {
                blocking = true;
                decisions.add(reject(value(pluginDTO, true), value(pluginDTO, false), INVALID_SOURCE));
                continue;
            }
            if (invalidPlugin(pluginDTO)) {
                blocking = true;
                decisions.add(reject(pluginDTO.getId(), pluginDTO.getName(), INVALID_PLUGIN));
                continue;
            }
            try {
                AgentGatewayPluginConfigValidator.validate(pluginDTO.getName(), pluginDTO.getConfig());
            } catch (ShenyuAdminException ex) {
                blocking = true;
                decisions.add(reject(pluginDTO.getId(), pluginDTO.getName(), INVALID_PLUGIN_CONFIG));
                continue;
            }
            ConfigImportDecision decision = resolver.resolve(pluginDTO);
            if (Objects.nonNull(decision.targetId())) {
                state.plannedPluginNamesById().put(decision.targetId(), pluginDTO.getName());
            }
            if (decision.action() == ConfigImportAction.CREATE) {
                decision = withDiagnostic(decision, GLOBAL_EFFECT);
            }
            decisions.add(decision);
        }
        return new SectionPlan(section(ExportImportConstants.PLUGIN_TEMPLATE_JSON, SCOPE_GLOBAL, blocking, VALIDATORS_PLUGIN, decisions, truncated), true);
    }

    private SectionPlan planNamespacePlugins(final String data, final PlanningState state) {
        final List<NamespacePluginDTO> pluginList;
        try {
            pluginList = parse(data, NamespacePluginDTO.class);
        } catch (RuntimeException ex) {
            return malformedSection(ExportImportConstants.NAMESPACE_PLUGIN_JSON, SCOPE_NAMESPACE, VALIDATORS_PLUGIN);
        }
        Map<String, NamespacePluginRelDO> existing = new LinkedHashMap<>();
        for (NamespacePluginRelDO relation : state.snapshot().namespacePlugins()) {
            existing.putIfAbsent(relation.getPluginId(), relation);
        }
        NamespacePluginImportResolver resolver = new NamespacePluginImportResolver(existing, state.context().getPluginTemplateIdMapping());
        List<ConfigImportDecision> decisions = new ArrayList<>();
        boolean blocking = false;
        boolean truncated = false;
        for (NamespacePluginDTO pluginDTO : pluginList) {
            if (!state.budget().tryConsume()) {
                truncated = true;
                break;
            }
            if (Objects.isNull(pluginDTO) || StringUtils.isBlank(pluginDTO.getPluginId())) {
                blocking = true;
                decisions.add(reject(value(pluginDTO, true), value(pluginDTO, false), INVALID_SOURCE));
                continue;
            }
            if (invalidNamespacePlugin(pluginDTO)) {
                blocking = true;
                decisions.add(reject(pluginDTO.getId(), pluginDTO.getName(), INVALID_NAMESPACE_PLUGIN));
                continue;
            }
            NamespacePluginImportResolver.Resolution resolution = resolver.resolve(pluginDTO);
            if (resolution.missingMapping()) {
                blocking = true;
                decisions.add(reject(pluginDTO.getId(), pluginDTO.getName(), MISSING_PLUGIN_TEMPLATE));
                continue;
            }
            String templateName = state.plannedPluginNamesById().get(resolution.mappedPluginId());
            if (Objects.isNull(templateName)) {
                PluginDO template = state.pluginsById().get(resolution.mappedPluginId());
                templateName = Objects.isNull(template) ? null : template.getName();
            }
            if (Objects.isNull(templateName)) {
                blocking = true;
                decisions.add(reject(pluginDTO.getId(), pluginDTO.getName(), MISSING_PLUGIN_TEMPLATE));
                continue;
            }
            try {
                AgentGatewayPluginConfigValidator.validate(pluginDTO.getName(), pluginDTO.getConfig());
                AgentGatewayPluginConfigValidator.validate(templateName, pluginDTO.getConfig());
            } catch (ShenyuAdminException ex) {
                blocking = true;
                decisions.add(reject(pluginDTO.getId(), pluginDTO.getName(), INVALID_PLUGIN_CONFIG));
                continue;
            }
            ConfigImportDecision decision;
            if (Objects.nonNull(resolution.existing())) {
                decision = new ConfigImportDecision(pluginDTO.getId(), pluginDTO.getName(), ConfigImportAction.SKIP,
                        resolution.existing().getId(), existingRef(resolution.existing().getId()),
                        List.of("plugin:" + pluginDTO.getPluginId() + " -> " + resolution.mappedPluginId()),
                        List.of(DUPLICATE_NAMESPACE_PLUGIN));
            } else {
                decision = new ConfigImportDecision(pluginDTO.getId(), pluginDTO.getName(), ConfigImportAction.CREATE,
                        resolution.mappedPluginId(), TARGET_NEW,
                        List.of("plugin:" + pluginDTO.getPluginId() + " -> " + resolution.mappedPluginId()),
                        List.of());
            }
            decisions.add(withNamespace(decision, pluginDTO.getNamespaceId(), state.namespace()));
        }
        return new SectionPlan(section(ExportImportConstants.NAMESPACE_PLUGIN_JSON, SCOPE_NAMESPACE, blocking, VALIDATORS_PLUGIN, decisions, truncated), true);
    }

    private SectionPlan planSelectors(final String data, final PlanningState state) {
        final List<SelectorDTO> selectorList;
        try {
            selectorList = parse(data, SelectorDTO.class);
        } catch (RuntimeException ex) {
            return malformedSection(ExportImportConstants.SELECTOR_JSON, SCOPE_NAMESPACE, VALIDATORS_SELECTOR);
        }
        Map<String, Map<String, String>> existing = new LinkedHashMap<>();
        for (SelectorDO selector : state.snapshot().selectors()) {
            existing.computeIfAbsent(selector.getPluginId(), key -> new LinkedHashMap<>())
                    .putIfAbsent(selector.getSelectorName(), selector.getId());
        }
        SelectorImportResolver resolver = new SelectorImportResolver(existing, state.context().getPluginTemplateIdMapping());
        List<ConfigImportDecision> decisions = new ArrayList<>();
        boolean blocking = false;
        boolean truncated = false;
        for (SelectorDTO selectorDTO : selectorList) {
            if (!state.budget().tryConsume()) {
                truncated = true;
                break;
            }
            if (Objects.isNull(selectorDTO) || StringUtils.isBlank(selectorDTO.getName())) {
                blocking = true;
                decisions.add(reject(value(selectorDTO, true), value(selectorDTO, false), INVALID_SOURCE));
                continue;
            }
            if (invalidSelector(selectorDTO)) {
                blocking = true;
                decisions.add(reject(selectorDTO.getId(), selectorDTO.getName(), INVALID_SELECTOR));
                continue;
            }
            SelectorImportResolver.Resolution resolution = resolver.resolve(selectorDTO);
            if (resolution.missingPlugin()) {
                blocking = true;
                decisions.add(reject(selectorDTO.getId(), selectorDTO.getName(), MISSING_PLUGIN_TEMPLATE));
                continue;
            }
            ConfigImportDecision decision;
            if (resolution.existing()) {
                state.context().getSelectorIdMapping().put(selectorDTO.getId(), resolution.existingSelectorId());
                String targetRef = resolver.isPlanned(resolution.existingSelectorId())
                        ? TARGET_PLANNED : existingRef(resolution.existingSelectorId());
                decision = new ConfigImportDecision(selectorDTO.getId(), selectorDTO.getName(), ConfigImportAction.REUSE,
                        resolution.existingSelectorId(), targetRef,
                        List.of("plugin:" + selectorDTO.getPluginId() + " -> " + resolution.pluginId(),
                                "selector:" + selectorDTO.getId() + " -> " + targetRef),
                        List.of(DUPLICATE_SELECTOR_NAME));
            } else {
                String selectorId = UUIDUtils.getInstance().generateShortUuid();
                resolver.reserve(selectorDTO, resolution.pluginId(), selectorId);
                state.context().getSelectorIdMapping().put(selectorDTO.getId(), selectorId);
                state.plannedSelectorPluginIds().put(selectorId, resolution.pluginId());
                decision = new ConfigImportDecision(selectorDTO.getId(), selectorDTO.getName(), ConfigImportAction.CREATE,
                        selectorId, TARGET_NEW,
                        List.of("plugin:" + selectorDTO.getPluginId() + " -> " + resolution.pluginId(),
                                "selector:" + selectorDTO.getId() + " -> " + TARGET_NEW),
                        List.of());
            }
            List<String> diagnostics = new ArrayList<>(decision.diagnostics());
            if (CollectionUtils.isNotEmpty(selectorDTO.getSelectorRules())) {
                diagnostics.add(EMBEDDED_RULES_IGNORED);
            }
            decision = withDiagnostics(decision, diagnostics);
            decisions.add(withNamespace(decision, selectorDTO.getNamespaceId(), state.namespace()));
        }
        return new SectionPlan(section(ExportImportConstants.SELECTOR_JSON, SCOPE_NAMESPACE, blocking, VALIDATORS_SELECTOR, decisions, truncated), true);
    }

    private SectionPlan planRules(final String data, final PlanningState state) {
        final List<RuleDTO> ruleList;
        try {
            ruleList = parse(data, RuleDTO.class);
        } catch (RuntimeException ex) {
            return malformedSection(ExportImportConstants.RULE_JSON, SCOPE_NAMESPACE, VALIDATORS_RULE);
        }
        Map<String, Map<String, String>> existing = new LinkedHashMap<>();
        for (RuleDO rule : state.snapshot().rules()) {
            existing.computeIfAbsent(rule.getSelectorId(), key -> new LinkedHashMap<>())
                    .putIfAbsent(rule.getRuleName(), rule.getId());
        }
        RuleImportResolver resolver = new RuleImportResolver(existing, state.context().getSelectorIdMapping());
        List<ConfigImportDecision> decisions = new ArrayList<>();
        boolean blocking = false;
        boolean truncated = false;
        for (RuleDTO ruleDTO : ruleList) {
            if (!state.budget().tryConsume()) {
                truncated = true;
                break;
            }
            if (Objects.isNull(ruleDTO) || StringUtils.isBlank(ruleDTO.getName()) || StringUtils.isBlank(ruleDTO.getSelectorId())) {
                blocking = true;
                decisions.add(reject(value(ruleDTO, true), value(ruleDTO, false), INVALID_SOURCE));
                continue;
            }
            if (invalidRule(ruleDTO)) {
                blocking = true;
                decisions.add(reject(ruleDTO.getId(), ruleDTO.getName(), INVALID_RULE));
                continue;
            }
            RuleImportResolver.Resolution resolution = resolver.resolve(ruleDTO);
            if (resolution.missingSelector()) {
                decisions.add(reject(ruleDTO.getId(), ruleDTO.getName(), MISSING_SELECTOR_REFERENCE));
                continue;
            }
            String pluginName = resolvePluginName(resolution.selectorId(), state);
            if (Objects.isNull(pluginName)) {
                blocking = true;
                decisions.add(reject(ruleDTO.getId(), ruleDTO.getName(), MISSING_PLUGIN_TEMPLATE));
                continue;
            }
            try {
                RuleImportValidator.validateCanaryConfig(pluginName, shenyuDictMapper, ruleDTO.getHandle());
            } catch (ShenyuAdminException ex) {
                blocking = true;
                decisions.add(reject(ruleDTO.getId(), ruleDTO.getName(), INVALID_CANARY_CONFIG));
                continue;
            }
            if (resolution.existing()) {
                String targetRef = resolver.isPlanned(resolution.existingRuleId())
                        ? TARGET_PLANNED : existingRef(resolution.existingRuleId());
                decisions.add(withNamespace(new ConfigImportDecision(ruleDTO.getId(), ruleDTO.getName(),
                        ConfigImportAction.SKIP, resolution.existingRuleId(), targetRef,
                        List.of("selector:" + ruleDTO.getSelectorId() + " -> " + resolution.selectorId()),
                        List.of(DUPLICATE_RULE_NAME)), ruleDTO.getNamespaceId(), state.namespace()));
                continue;
            }
            try {
                RuleImportValidator.validateRuleHandle(pluginName, ruleDTO.getHandle());
            } catch (ShenyuAdminException ex) {
                blocking = true;
                decisions.add(reject(ruleDTO.getId(), ruleDTO.getName(), INVALID_RULE_HANDLE));
                continue;
            }
            String ruleId = UUIDUtils.getInstance().generateShortUuid();
            resolver.reserve(resolution.selectorId(), ruleDTO.getName(), ruleId);
            decisions.add(withNamespace(new ConfigImportDecision(ruleDTO.getId(), ruleDTO.getName(),
                    ConfigImportAction.CREATE, ruleId, TARGET_NEW,
                    List.of("selector:" + ruleDTO.getSelectorId() + " -> " + resolution.selectorId(),
                            "rule:" + ruleDTO.getId() + " -> " + TARGET_NEW),
                    List.of()), ruleDTO.getNamespaceId(), state.namespace()));
        }
        return new SectionPlan(section(ExportImportConstants.RULE_JSON, SCOPE_NAMESPACE, blocking, VALIDATORS_RULE, decisions, truncated), true);
    }

    private String resolvePluginName(final String selectorId, final PlanningState state) {
        String pluginId = state.plannedSelectorPluginIds().get(selectorId);
        if (Objects.isNull(pluginId)) {
            pluginId = state.selectorPluginIds().get(selectorId);
        }
        if (Objects.isNull(pluginId)) {
            SelectorDO selector = selectorMapper.selectById(selectorId);
            if (Objects.nonNull(selector)) {
                pluginId = selector.getPluginId();
            }
        }
        if (Objects.isNull(pluginId)) {
            return null;
        }
        String pluginName = state.plannedPluginNamesById().get(pluginId);
        if (Objects.nonNull(pluginName)) {
            return pluginName;
        }
        PluginDO plugin = state.pluginsById().get(pluginId);
        if (Objects.nonNull(plugin)) {
            return plugin.getName();
        }
        PluginDO loaded = pluginMapper.selectById(pluginId);
        return Objects.isNull(loaded) ? null : loaded.getName();
    }

    private ConfigsImportPreview.SectionPreview section(final String name,
                                                        final String scope,
                                                        final boolean blocking,
                                                        final List<String> validators,
                                                        final List<ConfigImportDecision> decisions,
                                                        final boolean truncated) {
        List<ConfigsImportPreview.PreviewOutcome> outcomes = decisions.stream()
                .map(decision -> outcome(name, decision)).toList();
        List<String> diagnostics = truncated ? List.of(REPORT_TRUNCATED) : List.of();
        return new ConfigsImportPreview.SectionPreview(name, scope, true, true, blocking, validators, outcomes, diagnostics);
    }

    private ConfigsImportPreview.PreviewOutcome outcome(final String section, final ConfigImportDecision decision) {
        String sourceName = PreviewTextSanitizer.sanitize(decision.sourceName(), refLimit());
        String sourceId = PreviewTextSanitizer.sanitize(decision.sourceId(), refLimit());
        String sourceRef = section + ":" + sourceName + (StringUtils.isBlank(sourceId) ? "" : "#" + sourceId);
        String targetRef = PreviewTextSanitizer.sanitize(decision.targetRef(), refLimit());
        List<String> remapped = decision.remappedRefs().stream().limit(3)
                .map(value -> PreviewTextSanitizer.sanitize(value, refLimit())).toList();
        List<String> diagnostics = decision.diagnostics().stream()
                .limit(Math.max(1, maxDiagnosticsPerOutcome))
                .map(value -> PreviewTextSanitizer.sanitize(value, refLimit())).toList();
        return new ConfigsImportPreview.PreviewOutcome(sourceRef, sourceName, targetRef, decision.action().name(),
                remapped, diagnostics);
    }

    private SectionPlan malformedSection(final String name, final String scope, final List<String> validators) {
        return new SectionPlan(new ConfigsImportPreview.SectionPreview(name, scope, true, true, true,
                validators, List.of(), List.of(SECTION_MALFORMED)), false);
    }

    private ConfigsImportPreview.SectionPreview unsupportedSection(final String name, final String scope, final String diagnostic) {
        return new ConfigsImportPreview.SectionPreview(name, scope, false, true, false, List.of(), List.of(),
                List.of(diagnostic));
    }

    private static ConfigsImportPreview.Summary summarize(final List<ConfigsImportPreview.SectionPreview> sections) {
        int create = 0;
        int reuse = 0;
        int skip = 0;
        int reject = 0;
        for (ConfigsImportPreview.SectionPreview section : sections) {
            for (ConfigsImportPreview.PreviewOutcome outcome : section.outcomes()) {
                switch (outcome.action()) {
                    case "CREATE" -> create++;
                    case "REUSE" -> reuse++;
                    case "SKIP" -> skip++;
                    case "REJECT" -> reject++;
                    default -> {
                    }
                }
            }
        }
        return new ConfigsImportPreview.Summary(create, reuse, skip, reject);
    }

    private static List<String> warnings(final List<ConfigsImportPreview.SectionPreview> sections,
                                         final boolean blocking,
                                         final boolean truncated) {
        List<String> warnings = new ArrayList<>();
        warnings.add("OBSERVATION_ONLY: Preview reflects the target state at observedAt; it is not a reservation, approval, or proof of Gateway activation.");
        if (sections.stream().anyMatch(section -> ExportImportConstants.PLUGIN_TEMPLATE_JSON.equals(section.section()) && section.present())) {
            warnings.add("GLOBAL_PLUGIN_TEMPLATE: plugin_template.json affects global plugin templates, not only the target namespace.");
        }
        if (sections.stream().anyMatch(section -> !section.supported() && section.present())) {
            warnings.add("UNSUPPORTED_SECTIONS_PRESENT: Some archive sections were not analyzed; results are not fully validated.");
        }
        if (sections.stream().anyMatch(ConfigsImportPreviewServiceImpl::hasCoverageGap)) {
            warnings.add("COVERAGE_GAPS_PRESENT: Some objects could not be validated because a required dependency or identity is missing.");
        }
        if (blocking) {
            warnings.add("IMPORT_WOULD_FAIL_FAST: A blocking section would abort the live import; per-object outcomes are diagnostic only.");
        }
        if (truncated) {
            warnings.add("REPORT_TRUNCATED: The outcome budget stopped planning; later sections were not fully analyzed.");
        }
        warnings.add("PREVIEW_TOKEN: Pass previewToken to /configs/import to detect archive and baseline drift.");
        return warnings;
    }

    private static boolean hasCoverageGap(final ConfigsImportPreview.SectionPreview section) {
        return section.outcomes().stream().anyMatch(outcome -> outcome.diagnostics().contains(INVALID_SOURCE)
                || outcome.diagnostics().contains(INVALID_PLUGIN)
                || outcome.diagnostics().contains(INVALID_NAMESPACE_PLUGIN)
                || outcome.diagnostics().contains(INVALID_SELECTOR)
                || outcome.diagnostics().contains(INVALID_RULE)
                || outcome.diagnostics().contains(MISSING_PLUGIN_TEMPLATE)
                || outcome.diagnostics().contains(MISSING_SELECTOR_REFERENCE));
    }

    private static boolean invalidPlugin(final PluginDTO pluginDTO) {
        return StringUtils.isBlank(pluginDTO.getRole()) || Objects.isNull(pluginDTO.getEnabled());
    }

    private static boolean invalidNamespacePlugin(final NamespacePluginDTO pluginDTO) {
        return Objects.isNull(pluginDTO.getEnabled());
    }

    private static boolean invalidSelector(final SelectorDTO selectorDTO) {
        if (Objects.isNull(selectorDTO.getType()) || Objects.isNull(selectorDTO.getSort())
                || Objects.isNull(selectorDTO.getEnabled()) || Objects.isNull(selectorDTO.getLogged())
                || Objects.isNull(selectorDTO.getContinued()) || Objects.isNull(selectorDTO.getMatchRestful())
                || Objects.isNull(selectorDTO.getSelectorConditions())) {
            return true;
        }
        return !Objects.equals(SelectorTypeEnum.FULL_FLOW.getCode(), selectorDTO.getType())
                && Objects.isNull(selectorDTO.getMatchMode());
    }

    private static boolean invalidRule(final RuleDTO ruleDTO) {
        return Objects.isNull(ruleDTO.getMatchMode()) || Objects.isNull(ruleDTO.getSort())
                || Objects.isNull(ruleDTO.getEnabled()) || Objects.isNull(ruleDTO.getLogged())
                || Objects.isNull(ruleDTO.getMatchRestful()) || Objects.isNull(ruleDTO.getRuleConditions());
    }

    private static boolean isSupported(final ConfigsExportImportEnum section) {
        return switch (section) {
            case PluginTemplate, NamespacePlugin, Selector, Rule -> true;
            default -> false;
        };
    }

    private static String scopeOf(final ConfigsExportImportEnum section) {
        return switch (section) {
            case PluginTemplate, PluginHandle, Dict -> SCOPE_GLOBAL;
            default -> SCOPE_NAMESPACE;
        };
    }

    private static ConfigImportDecision reject(final String sourceId, final String sourceName, final String diagnostic) {
        return new ConfigImportDecision(sourceId, sourceName, ConfigImportAction.REJECT, null, TARGET_NONE,
                List.of(), List.of(diagnostic));
    }

    private static String existingRef(final String id) {
        return Objects.isNull(id) ? "existing" : TARGET_EXISTING + id;
    }

    private static ConfigImportDecision withDiagnostic(final ConfigImportDecision decision, final String diagnostic) {
        List<String> diagnostics = new ArrayList<>(decision.diagnostics());
        diagnostics.add(diagnostic);
        return withDiagnostics(decision, diagnostics);
    }

    private static ConfigImportDecision withDiagnostics(final ConfigImportDecision decision, final List<String> diagnostics) {
        return new ConfigImportDecision(decision.sourceId(), decision.sourceName(), decision.action(), decision.targetId(),
                decision.targetRef(), decision.remappedRefs(), diagnostics);
    }

    private static ConfigImportDecision withNamespace(final ConfigImportDecision decision,
                                                      final String sourceNamespace,
                                                      final String targetNamespace) {
        if (StringUtils.isBlank(sourceNamespace) || Objects.equals(sourceNamespace, targetNamespace)) {
            return decision;
        }
        List<String> remapped = new ArrayList<>(decision.remappedRefs());
        remapped.add("namespace:" + sourceNamespace + " -> " + targetNamespace);
        List<String> diagnostics = new ArrayList<>(decision.diagnostics());
        diagnostics.add(NAMESPACE_REMAPPED);
        return new ConfigImportDecision(decision.sourceId(), decision.sourceName(), decision.action(), decision.targetId(),
                decision.targetRef(), remapped, diagnostics);
    }

    private static String value(final Object dto, final boolean id) {
        if (Objects.isNull(dto)) {
            return null;
        }
        if (dto instanceof PluginDTO pluginDTO) {
            return id ? pluginDTO.getId() : pluginDTO.getName();
        }
        if (dto instanceof NamespacePluginDTO pluginDTO) {
            return id ? pluginDTO.getId() : pluginDTO.getName();
        }
        if (dto instanceof SelectorDTO selectorDTO) {
            return id ? selectorDTO.getId() : selectorDTO.getName();
        }
        if (dto instanceof RuleDTO ruleDTO) {
            return id ? ruleDTO.getId() : ruleDTO.getName();
        }
        return null;
    }

    private static <T> List<T> parse(final String data, final Class<T> type) {
        List<T> result = GsonUtils.getInstance().fromList(data, type);
        return Objects.isNull(result) ? List.of() : result;
    }

    private int refLimit() {
        return Math.max(1, maxRefLength);
    }

    private Semaphore semaphore() {
        Semaphore current = previewSemaphore;
        if (Objects.isNull(current)) {
            synchronized (this) {
                if (Objects.isNull(previewSemaphore)) {
                    previewSemaphore = new Semaphore(Math.max(1, maxConcurrent));
                }
                current = previewSemaphore;
            }
        }
        return current;
    }

    private record SectionPlan(ConfigsImportPreview.SectionPreview section, boolean parsed) {
    }

    private record PlanningState(String namespace,
                                 ConfigsImportTargetSnapshot snapshot,
                                 ConfigsImportContext context,
                                 PreviewBudget budget,
                                 Map<String, PluginDO> pluginsById,
                                 Map<String, String> selectorPluginIds,
                                 Map<String, String> plannedPluginNamesById,
                                 Map<String, String> plannedSelectorPluginIds) {
    }

    private record PreviewPlanning(List<ConfigsImportPreview.SectionPreview> sections,
                                   boolean coverage,
                                   boolean blocking,
                                   boolean truncated) {
    }

    private static final class PreviewBudget {

        private int remaining;

        private PreviewBudget(final int maxOutcomes) {
            this.remaining = Math.max(0, maxOutcomes);
        }

        private boolean tryConsume() {
            if (remaining <= 0) {
                return false;
            }
            remaining--;
            return true;
        }

        private boolean isExhausted() {
            return remaining <= 0;
        }
    }
}
