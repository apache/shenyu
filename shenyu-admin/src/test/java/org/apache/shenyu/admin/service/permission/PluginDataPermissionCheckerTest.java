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

package org.apache.shenyu.admin.service.permission;

import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.utils.ResourceUtil;
import org.apache.shiro.authz.UnauthorizedException;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.ThreadContext;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.function.Executable;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link PluginDataPermissionChecker}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class PluginDataPermissionCheckerTest {

    private static final String DIVIDE = "divide";

    private static final String GRPC = "grpc";

    private static final String[] LEGACY_GLOBAL_GRANTS = {"system:plugin:edit", "system:plugin:disable", "system:plugin:delete"};

    private final Map<String, SelectorDO> selectors = new HashMap<>();

    private final Map<String, RuleDO> rules = new HashMap<>();

    private final Map<String, PluginDO> plugins = new HashMap<>();

    @InjectMocks
    private PluginDataPermissionChecker checker;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private RuleMapper ruleMapper;

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private Subject subject;

    @BeforeEach
    public void setUp() {
        ThreadContext.bind(subject);
        addPlugin(DIVIDE);
        addPlugin(GRPC);
        selectors.put("orphan-selector", SelectorDO.builder().id("orphan-selector").pluginId("missing-plugin").build());
        rules.put("orphan-rule", RuleDO.builder().id("orphan-rule").selectorId("missing-selector").build());
        when(selectorMapper.selectByIdSet(anySet())).thenAnswer(invocation -> lookup(invocation.getArgument(0), selectors));
        when(ruleMapper.selectByIds(anyList())).thenAnswer(invocation -> lookup(invocation.getArgument(0), rules));
        when(pluginMapper.selectByIds(anyList())).thenAnswer(invocation -> lookup(invocation.getArgument(0), plugins));
        grant();
    }

    @AfterEach
    public void tearDown() {
        ThreadContext.unbindSubject();
    }

    @Test
    public void legacyGlobalRoleCanWriteEveryPluginWithoutLookup() {
        grant(LEGACY_GLOBAL_GRANTS);
        assertAllowed(writesOn(DIVIDE));
        assertAllowed(writesOn(GRPC));
        verifyNoInteractions(selectorMapper, ruleMapper, pluginMapper);
    }

    @Test
    public void divideOnlyRoleCanWriteDivideButNotGrpc() {
        grant(generatedPermissionsOf(DIVIDE));
        assertAllowed(writesOn(DIVIDE));
        assertDenied(writesOn(GRPC));
    }

    @Test
    public void roleWithBothGrantsCanWriteEveryPlugin() {
        final Set<String> permissions = new HashSet<>(Arrays.asList(LEGACY_GLOBAL_GRANTS));
        permissions.addAll(Arrays.asList(generatedPermissionsOf(DIVIDE)));
        grant(permissions.toArray(new String[0]));
        assertAllowed(writesOn(DIVIDE));
        assertAllowed(writesOn(GRPC));
    }

    @Test
    public void readOnlyRoleCannotWriteAnyPlugin() {
        grant("system:plugin:list", "plugin:divideSelector:query", "plugin:divideRule:query");
        assertDenied(writesOn(DIVIDE));
        assertDenied(writesOn(GRPC));
    }

    @Test
    public void globalGrantOnlyCoversItsOwnEndpoints() {
        grant("system:plugin:edit");
        assertAllowed(Arrays.asList(createSelector(DIVIDE), updateSelector(DIVIDE), createRule(DIVIDE), updateRule(DIVIDE)));
        assertDenied(Arrays.asList(enableSelector(DIVIDE), enableRule(DIVIDE), deleteSelector(DIVIDE), deleteRule(DIVIDE)));

        grant("system:plugin:disable");
        assertAllowed(Arrays.asList(enableSelector(DIVIDE), enableRule(DIVIDE)));
        assertDenied(Arrays.asList(createSelector(DIVIDE), updateSelector(DIVIDE), deleteSelector(DIVIDE), createRule(DIVIDE), updateRule(DIVIDE), deleteRule(DIVIDE)));

        grant("system:plugin:delete");
        assertAllowed(Arrays.asList(deleteSelector(DIVIDE), deleteRule(DIVIDE)));
        assertDenied(Arrays.asList(createSelector(DIVIDE), updateSelector(DIVIDE), enableSelector(DIVIDE), createRule(DIVIDE), updateRule(DIVIDE), enableRule(DIVIDE)));

        // the plugin management grant does not create selector or rule data
        grant("system:plugin:add");
        assertDenied(writesOn(DIVIDE));
    }

    @Test
    public void pluginGrantOnlyCoversItsOwnActions() {
        grant("plugin:divideSelector:add", "plugin:divideRule:add");
        assertAllowed(Arrays.asList(createSelector(DIVIDE), createRule(DIVIDE)));
        assertDenied(Arrays.asList(updateSelector(DIVIDE), enableSelector(DIVIDE), deleteSelector(DIVIDE), updateRule(DIVIDE), enableRule(DIVIDE), deleteRule(DIVIDE)));

        // enable and disable reuse the edit grant
        grant("plugin:divideSelector:edit", "plugin:divideRule:edit");
        assertAllowed(Arrays.asList(updateSelector(DIVIDE), enableSelector(DIVIDE), updateRule(DIVIDE), enableRule(DIVIDE)));
        assertDenied(Arrays.asList(createSelector(DIVIDE), deleteSelector(DIVIDE), createRule(DIVIDE), deleteRule(DIVIDE)));

        grant("plugin:divideSelector:delete", "plugin:divideRule:delete");
        assertAllowed(Arrays.asList(deleteSelector(DIVIDE), deleteRule(DIVIDE)));
        assertDenied(Arrays.asList(createSelector(DIVIDE), updateSelector(DIVIDE), enableSelector(DIVIDE), createRule(DIVIDE), updateRule(DIVIDE), enableRule(DIVIDE)));
    }

    @Test
    public void selectorGrantDoesNotCoverRulesAndViceVersa() {
        grant("plugin:divideSelector:edit");
        assertAllowed(Collections.singletonList(updateSelector(DIVIDE)));
        assertDenied(Collections.singletonList(updateRule(DIVIDE)));

        grant("plugin:divideRule:edit");
        assertAllowed(Collections.singletonList(updateRule(DIVIDE)));
        assertDenied(Collections.singletonList(updateSelector(DIVIDE)));
    }

    @Test
    public void batchIsDeniedWhenAnyItemBelongsToAnotherPlugin() {
        grant(generatedPermissionsOf(DIVIDE));
        final List<String> selectorIds = Arrays.asList(selectorOf(DIVIDE), selectorOf(GRPC));
        final List<String> ruleIds = Arrays.asList(ruleOf(DIVIDE), ruleOf(GRPC));
        assertDenied(Arrays.asList(
            () -> checker.checkEnableSelectors(selectorIds),
            () -> checker.checkDeleteSelectors(selectorIds),
            () -> checker.checkEnableRules(ruleIds),
            () -> checker.checkDeleteRules(ruleIds)));
    }

    @Test
    public void repeatedIdsInBatchAreCheckedOnce() {
        grant(generatedPermissionsOf(DIVIDE));
        assertAllowed(Arrays.asList(
            () -> checker.checkDeleteSelectors(Arrays.asList(selectorOf(DIVIDE), selectorOf(DIVIDE))),
            () -> checker.checkDeleteRules(Arrays.asList(ruleOf(DIVIDE), ruleOf(DIVIDE)))));
    }

    @Test
    public void unresolvedPluginIsOnlyAllowedByGlobalGrant() {
        final List<Executable> unresolved = Arrays.asList(
            () -> checker.checkCreateSelector(SelectorDTO.builder().pluginId("missing-plugin").build()),
            () -> checker.checkUpdateSelector("missing-selector"),
            () -> checker.checkUpdateSelector("orphan-selector"),
            () -> checker.checkDeleteSelectors(Arrays.asList(selectorOf(DIVIDE), "missing-selector")),
            () -> checker.checkEnableSelectors(Collections.emptyList()),
            () -> checker.checkCreateRule(RuleDTO.builder().selectorId("missing-selector").build()),
            () -> checker.checkUpdateRule("missing-rule"),
            () -> checker.checkUpdateRule("orphan-rule"),
            () -> checker.checkDeleteRules(Arrays.asList(ruleOf(DIVIDE), "missing-rule")),
            () -> checker.checkEnableRules(Collections.emptyList()));
        grant(generatedPermissionsOf(DIVIDE));
        assertDenied(unresolved);

        grant(LEGACY_GLOBAL_GRANTS);
        assertAllowed(unresolved);
    }

    @Test
    public void createRequestCarryingAnIdIsCheckedAsAnUpdateOfThePersistedObject() {
        final Executable createSelectorWithDivideId = () -> checker.checkCreateSelector(SelectorDTO.builder().id(selectorOf(DIVIDE)).pluginId(idOf(DIVIDE)).build());
        final Executable createRuleWithDivideId = () -> checker.checkCreateRule(RuleDTO.builder().id(ruleOf(DIVIDE)).selectorId(selectorOf(DIVIDE)).build());
        grant("plugin:divideSelector:add", "plugin:divideRule:add");
        assertDenied(Arrays.asList(createSelectorWithDivideId, createRuleWithDivideId));

        grant("plugin:divideSelector:edit", "plugin:divideRule:edit");
        assertAllowed(Arrays.asList(createSelectorWithDivideId, createRuleWithDivideId));

        // the request body cannot redirect the check away from the plugin of the persisted object
        assertDenied(Arrays.asList(
            () -> checker.checkCreateSelector(SelectorDTO.builder().id(selectorOf(GRPC)).pluginId(idOf(DIVIDE)).build()),
            () -> checker.checkCreateRule(RuleDTO.builder().id(ruleOf(GRPC)).selectorId(selectorOf(DIVIDE)).build())));
    }

    @Test
    public void blankIdFollowsTheRoutingOfCreateOrUpdate() {
        // SelectorService routes on isEmpty, so a blank id is an update of a selector that does not exist
        grant("plugin:divideSelector:add");
        assertDenied(Collections.singletonList(() -> checker.checkCreateSelector(SelectorDTO.builder().id(" ").pluginId(idOf(DIVIDE)).build())));
        assertAllowed(Collections.singletonList(() -> checker.checkCreateSelector(SelectorDTO.builder().id("").pluginId(idOf(DIVIDE)).build())));

        // RuleService routes on isBlank, so a blank id is a create
        grant("plugin:divideRule:add");
        assertAllowed(Collections.singletonList(() -> checker.checkCreateRule(RuleDTO.builder().id(" ").selectorId(selectorOf(DIVIDE)).build())));
    }

    private void addPlugin(final String name) {
        plugins.put(idOf(name), PluginDO.builder().id(idOf(name)).name(name).build());
        selectors.put(selectorOf(name), SelectorDO.builder().id(selectorOf(name)).pluginId(idOf(name)).build());
        rules.put(ruleOf(name), RuleDO.builder().id(ruleOf(name)).selectorId(selectorOf(name)).build());
    }

    private void grant(final String... permissions) {
        final Set<String> granted = new HashSet<>(Arrays.asList(permissions));
        when(subject.isPermitted(anyString())).thenAnswer(invocation -> granted.contains(invocation.<String>getArgument(0)));
    }

    private String[] generatedPermissionsOf(final String plugin) {
        return ResourceUtil.buildPluginDataPermissionResource("parent", plugin).stream()
                .map(ResourceDO::getPerms)
                .toArray(String[]::new);
    }

    private List<Executable> writesOn(final String plugin) {
        return Arrays.asList(createSelector(plugin), updateSelector(plugin), enableSelector(plugin), deleteSelector(plugin),
                createRule(plugin), updateRule(plugin), enableRule(plugin), deleteRule(plugin));
    }

    private Executable createSelector(final String plugin) {
        return () -> checker.checkCreateSelector(SelectorDTO.builder().pluginId(idOf(plugin)).build());
    }

    private Executable updateSelector(final String plugin) {
        return () -> checker.checkUpdateSelector(selectorOf(plugin));
    }

    private Executable enableSelector(final String plugin) {
        return () -> checker.checkEnableSelectors(Collections.singletonList(selectorOf(plugin)));
    }

    private Executable deleteSelector(final String plugin) {
        return () -> checker.checkDeleteSelectors(Collections.singletonList(selectorOf(plugin)));
    }

    private Executable createRule(final String plugin) {
        return () -> checker.checkCreateRule(RuleDTO.builder().selectorId(selectorOf(plugin)).build());
    }

    private Executable updateRule(final String plugin) {
        return () -> checker.checkUpdateRule(ruleOf(plugin));
    }

    private Executable enableRule(final String plugin) {
        return () -> checker.checkEnableRules(Collections.singletonList(ruleOf(plugin)));
    }

    private Executable deleteRule(final String plugin) {
        return () -> checker.checkDeleteRules(Collections.singletonList(ruleOf(plugin)));
    }

    private static String idOf(final String plugin) {
        return plugin + "-id";
    }

    private static String selectorOf(final String plugin) {
        return plugin + "-selector";
    }

    private static String ruleOf(final String plugin) {
        return plugin + "-rule";
    }

    private static <T> List<T> lookup(final Collection<String> ids, final Map<String, T> table) {
        return new ArrayList<>(ids).stream().map(table::get).filter(Objects::nonNull).collect(Collectors.toList());
    }

    private static void assertAllowed(final List<Executable> writes) {
        writes.forEach(write -> assertDoesNotThrow(write));
    }

    private static void assertDenied(final List<Executable> writes) {
        writes.forEach(write -> assertThrows(UnauthorizedException.class, write));
    }
}
