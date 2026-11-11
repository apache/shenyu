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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.utils.ResourceUtil;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shiro.SecurityUtils;
import org.apache.shiro.authz.UnauthorizedException;
import org.apache.shiro.subject.Subject;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * Authorizes selector and rule writes.
 *
 * <p>A write is allowed when the user holds the global {@code system:plugin:*} grant of the action,
 * or the plugin-specific {@code plugin:<name><Selector|Rule>:<action>} grant of every plugin it touches.
 * The plugin comes from the persisted object when one exists, and a plugin that cannot be resolved is
 * only permitted through the global grant.
 */
@Service
public class PluginDataPermissionChecker {

    private static final String GLOBAL_EDIT = "system:plugin:edit";

    private static final String GLOBAL_DISABLE = "system:plugin:disable";

    private static final String GLOBAL_DELETE = "system:plugin:delete";

    private final SelectorMapper selectorMapper;

    private final RuleMapper ruleMapper;

    private final PluginMapper pluginMapper;

    public PluginDataPermissionChecker(final SelectorMapper selectorMapper,
                                       final RuleMapper ruleMapper,
                                       final PluginMapper pluginMapper) {
        this.selectorMapper = selectorMapper;
        this.ruleMapper = ruleMapper;
        this.pluginMapper = pluginMapper;
    }

    /**
     * Check the create selector request.
     *
     * @param selectorDTO selector
     */
    public void checkCreateSelector(final SelectorDTO selectorDTO) {
        // createOrUpdate turns a request carrying an id into an update of the persisted selector
        if (!StringUtils.isEmpty(selectorDTO.getId())) {
            checkUpdateSelector(selectorDTO.getId());
            return;
        }
        check(GLOBAL_EDIT, AdminConstants.PLUGIN_TYPE_SELECTOR_ADD, () -> pluginNames(Collections.singleton(selectorDTO.getPluginId())));
    }

    /**
     * Check the update selector request.
     *
     * @param id selector id
     */
    public void checkUpdateSelector(final String id) {
        check(GLOBAL_EDIT, AdminConstants.PLUGIN_TYPE_SELECTOR_EDIT, () -> pluginNamesOfSelectors(Collections.singleton(id)));
    }

    /**
     * Check the batch enable selector request.
     *
     * @param ids selector ids
     */
    public void checkEnableSelectors(final List<String> ids) {
        check(GLOBAL_DISABLE, AdminConstants.PLUGIN_TYPE_SELECTOR_EDIT, () -> pluginNamesOfSelectors(ids));
    }

    /**
     * Check the batch delete selector request.
     *
     * @param ids selector ids
     */
    public void checkDeleteSelectors(final List<String> ids) {
        check(GLOBAL_DELETE, AdminConstants.PLUGIN_TYPE_SELECTOR_DELETE, () -> pluginNamesOfSelectors(ids));
    }

    /**
     * Check the create rule request.
     *
     * @param ruleDTO rule
     */
    public void checkCreateRule(final RuleDTO ruleDTO) {
        // createOrUpdate turns a request carrying an id into an update of the persisted rule
        if (!StringUtils.isBlank(ruleDTO.getId())) {
            checkUpdateRule(ruleDTO.getId());
            return;
        }
        check(GLOBAL_EDIT, AdminConstants.PLUGIN_TYPE_RULE_ADD, () -> pluginNamesOfSelectors(Collections.singleton(ruleDTO.getSelectorId())));
    }

    /**
     * Check the update rule request.
     *
     * @param id rule id
     */
    public void checkUpdateRule(final String id) {
        check(GLOBAL_EDIT, AdminConstants.PLUGIN_TYPE_RULE_EDIT, () -> pluginNamesOfRules(Collections.singleton(id)));
    }

    /**
     * Check the batch enable rule request.
     *
     * @param ids rule ids
     */
    public void checkEnableRules(final List<String> ids) {
        check(GLOBAL_DISABLE, AdminConstants.PLUGIN_TYPE_RULE_EDIT, () -> pluginNamesOfRules(ids));
    }

    /**
     * Check the batch delete rule request.
     *
     * @param ids rule ids
     */
    public void checkDeleteRules(final List<String> ids) {
        check(GLOBAL_DELETE, AdminConstants.PLUGIN_TYPE_RULE_DELETE, () -> pluginNamesOfRules(ids));
    }

    private void check(final String globalPermission, final String pluginAction, final Supplier<Set<String>> pluginNames) {
        final Subject subject = SecurityUtils.getSubject();
        if (subject.isPermitted(globalPermission)) {
            return;
        }
        final Set<String> names = pluginNames.get();
        if (names.isEmpty() || !names.stream().allMatch(name -> subject.isPermitted(ResourceUtil.buildPluginPermission(name, pluginAction)))) {
            throw new UnauthorizedException("Subject does not have the permission of the plugin data: " + pluginAction);
        }
    }

    private Set<String> pluginNamesOfRules(final Collection<String> ruleIds) {
        final List<String> ids = new ArrayList<>(new HashSet<>(ruleIds));
        if (ids.isEmpty()) {
            return Collections.emptySet();
        }
        final List<RuleDO> rules = ruleMapper.selectByIds(ids);
        if (rules.size() != ids.size()) {
            return Collections.emptySet();
        }
        return pluginNamesOfSelectors(rules.stream().map(RuleDO::getSelectorId).collect(Collectors.toSet()));
    }

    private Set<String> pluginNamesOfSelectors(final Collection<String> selectorIds) {
        final Set<String> ids = new HashSet<>(selectorIds);
        if (ids.isEmpty()) {
            return Collections.emptySet();
        }
        final List<SelectorDO> selectors = selectorMapper.selectByIdSet(ids);
        if (selectors.size() != ids.size()) {
            return Collections.emptySet();
        }
        return pluginNames(selectors.stream().map(SelectorDO::getPluginId).collect(Collectors.toSet()));
    }

    private Set<String> pluginNames(final Set<String> pluginIds) {
        final List<PluginDO> plugins = pluginMapper.selectByIds(new ArrayList<>(pluginIds));
        if (plugins.size() != pluginIds.size()) {
            return Collections.emptySet();
        }
        return plugins.stream().map(PluginDO::getName).collect(Collectors.toSet());
    }
}
