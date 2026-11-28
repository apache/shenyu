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

package org.apache.shenyu.admin.service.support;

import com.google.common.collect.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorConditionDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.transfer.ConditionTransfer;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * Reserves enabled selector scopes independently of mutable rules and namespace plugin switches.
 */
public final class AiPassthroughSelectorGuard {

    private static final String PROXY = PluginEnum.AI_PROXY.getName();

    private static final Set<String> PATH_REWRITERS = Set.of(PluginEnum.CONTEXT_PATH.getName(), PluginEnum.REWRITE.getName(),
            PluginEnum.REDIRECT.getName(), PluginEnum.RESILIENCE4J.getName(), PluginEnum.SENTINEL.getName());

    private static final List<String> POLICY_PLUGINS = List.of(PROXY, PluginEnum.AI_PROMPT.getName(),
            PluginEnum.AI_REQUEST_TRANSFORMER.getName(), PluginEnum.AI_RESPONSE_TRANSFORMER.getName(),
            PluginEnum.PII_DETECTION.getName(), PluginEnum.SENSITIVE_WORD.getName(), PluginEnum.CONTEXT_PATH.getName(), PluginEnum.REWRITE.getName(),
            PluginEnum.REDIRECT.getName(), PluginEnum.RESILIENCE4J.getName(), PluginEnum.SENTINEL.getName());

    private static final Pattern LITERAL_PATH_SEGMENT = Pattern.compile("[A-Za-z0-9_-]+");

    // Keep IN lists below Oracle's 1000-expression limit.
    private static final int CONDITION_BATCH_SIZE = 500;

    private final PluginMapper plugins;

    private final SelectorMapper selectors;

    private final SelectorConditionMapper conditions;

    public AiPassthroughSelectorGuard(final PluginMapper plugins, final SelectorMapper selectors, final SelectorConditionMapper conditions) {
        this.plugins = plugins;
        this.selectors = selectors;
        this.conditions = conditions;
    }

    /**
     * Validate the proposed selector within the writing transaction.
     *
     * @param change selector fields supplied by the caller
     * @param before previous selector, or the proposed selector on creation
     * @param replacement new conditions, or null when existing conditions are retained
     */
    public void validate(final SelectorDO change, final SelectorDO before, final List<SelectorConditionDTO> replacement) {
        if (Objects.isNull(before)) {
            return;
        }
        String pluginId = StringUtils.defaultIfEmpty(change.getPluginId(), before.getPluginId());
        PluginDO owner = plugins.selectById(pluginId);
        if (Objects.isNull(owner) || Objects.isNull(owner.getName()) || !POLICY_PLUGINS.contains(owner.getName())) {
            return;
        }
        AiProxySelectorValidation.validateHandle(owner.getName(), change.getHandle());
        if (Boolean.FALSE.equals(change.getEnabled())) {
            return;
        }
        // The common row serializes opposite-side edits even when neither selector exists yet.
        // Locking reads below also see those commits under MySQL's repeatable-read isolation.
        if (Objects.isNull(plugins.selectByNameForUpdate(PROXY))) {
            return;
        }
        Map<String, String> names = plugins.selectByNames(POLICY_PLUGINS).stream()
                .collect(Collectors.toMap(PluginDO::getId, PluginDO::getName));
        names.put(pluginId, owner.getName());
        String namespace = StringUtils.defaultIfEmpty(before.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID);
        Map<String, SelectorDO> rows = selectors.findByPluginIdsAndNamespaceIdForUpdate(new ArrayList<>(names.keySet()), namespace).stream()
                .collect(Collectors.toMap(SelectorDO::getId, Function.identity()));
        SelectorDO stored = rows.getOrDefault(change.getId(), before);
        SelectorDO candidate = SelectorDO.builder().id(change.getId()).pluginId(pluginId)
                .enabled(Objects.nonNull(change.getEnabled()) ? change.getEnabled() : stored.getEnabled())
                .handle(Objects.nonNull(change.getHandle()) ? change.getHandle() : stored.getHandle())
                .selectorType(Objects.nonNull(change.getSelectorType()) ? change.getSelectorType() : stored.getSelectorType())
                .matchRestful(Objects.nonNull(change.getMatchRestful()) ? change.getMatchRestful() : stored.getMatchRestful()).build();
        if (Objects.isNull(change.getHandle())) {
            AiProxySelectorValidation.validateHandle(owner.getName(), candidate.getHandle());
        }
        rows.put(candidate.getId(), candidate);
        List<SelectorDO> enabled = rows.values().stream().filter(row -> !Boolean.FALSE.equals(row.getEnabled())).toList();
        List<SelectorDO> nativeRoutes = enabled.stream().filter(row -> PROXY.equals(names.get(row.getPluginId())) && isPassthrough(row)).toList();
        if (nativeRoutes.isEmpty()) {
            return;
        }
        List<SelectorDO> bodyPolicies = enabled.stream().filter(row -> {
            String name = names.get(row.getPluginId());
            return !PROXY.equals(name) && !PATH_REWRITERS.contains(name);
        }).toList();
        if (bodyPolicies.isEmpty()) {
            return;
        }
        Map<String, List<ConditionData>> predicates = new HashMap<>();
        List<String> conditionIds = Stream.concat(nativeRoutes.stream(), bodyPolicies.stream()).map(SelectorDO::getId)
                .filter(id -> Objects.isNull(replacement) || !id.equals(candidate.getId())).toList();
        for (List<String> batch : Lists.partition(conditionIds, CONDITION_BATCH_SIZE)) {
            conditions.selectBySelectorIdsForUpdate(batch).forEach(condition -> addCondition(predicates, condition));
        }
        if (Objects.nonNull(replacement)) {
            predicates.put(candidate.getId(), replacement.stream().map(ConditionTransfer.INSTANCE::mapToSelectorDTO).toList());
        }
        boolean stablePath = enabled.stream().map(row -> names.get(row.getPluginId()))
                .noneMatch(name -> PATH_REWRITERS.contains(name) || PluginEnum.AI_REQUEST_TRANSFORMER.getName().equals(name));
        assertDisjoint(stablePath, bodyPolicies, nativeRoutes, names, predicates);
    }

    private static void assertDisjoint(final boolean stablePath, final List<SelectorDO> bodyPolicies, final List<SelectorDO> nativeRoutes,
                                       final Map<String, String> names, final Map<String, List<ConditionData>> predicates) {
        for (SelectorDO policy : bodyPolicies) {
            String policyPrefix = literalPathPrefix(policy, predicates);
            for (SelectorDO route : nativeRoutes) {
                String routePrefix = literalPathPrefix(route, predicates);
                if (!stablePath || routePrefix.startsWith(policyPrefix) || policyPrefix.startsWith(routePrefix)) {
                    throw new ShenyuAdminException("AI passthrough selector " + route.getId() + " may overlap " + names.get(policy.getPluginId()) + " selector " + policy.getId());
                }
            }
        }
    }

    private static boolean isPassthrough(final SelectorDO row) {
        if (StringUtils.isEmpty(row.getHandle())) {
            return false;
        }
        try {
            return AiProxyHandle.fromSelectorHandle(row.getHandle()).isPassthrough();
        } catch (IllegalArgumentException e) {
            // An existing malformed handle cannot route at the gateway; validate its replacement on save.
            return false;
        }
    }

    private static void addCondition(final Map<String, List<ConditionData>> predicates, final SelectorConditionDO condition) {
        predicates.computeIfAbsent(condition.getSelectorId(), key -> new ArrayList<>()).add(ConditionTransfer.INSTANCE.mapToSelectorDO(condition));
    }

    private static String literalPathPrefix(final SelectorDO selector, final Map<String, List<ConditionData>> predicates) {
        List<ConditionData> list = predicates.getOrDefault(selector.getId(), List.of());
        if (!Objects.equals(selector.getSelectorType(), SelectorTypeEnum.CUSTOM_FLOW.getCode()) || Boolean.TRUE.equals(selector.getMatchRestful()) || list.size() != 1) {
            return "/";
        }
        ConditionData condition = list.get(0);
        if (!ParamTypeEnum.URI.getName().equals(condition.getParamType()) || Objects.isNull(condition.getParamValue())) {
            return "/";
        }
        String path = condition.getParamValue().trim();
        String operator = condition.getOperator();
        if (OperatorEnum.STARTS_WITH.getAlias().equals(operator)) {
            path = StringUtils.substringBeforeLast(path, "/");
        } else if (!OperatorEnum.EQ.getAlias().equals(operator) && !OperatorEnum.MATCH.getAlias().equals(operator)) {
            return "/";
        }
        StringBuilder prefix = new StringBuilder("/");
        if (path.startsWith("/")) {
            for (String segment : path.substring(1).split("/", -1)) {
                if (!LITERAL_PATH_SEGMENT.matcher(segment).matches()) {
                    break;
                }
                prefix.append(segment).append('/');
            }
        }
        return prefix.toString();
    }
}
