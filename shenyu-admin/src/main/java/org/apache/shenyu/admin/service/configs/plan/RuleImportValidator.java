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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.mapper.ShenyuDictMapper;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.validation.validator.AgentGatewayRuleHandleValidator;
import org.apache.shenyu.admin.validation.validator.RewriteRuleHandleValidator;
import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfigValidator;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.dto.convert.rule.sticky.StickySessionConfigValidator;
import org.apache.shenyu.common.enums.PluginEnum;

import java.util.Objects;

/**
 * Rule validation shared by the import write path and the read-only preview.
 */
public final class RuleImportValidator {

    private RuleImportValidator() {
    }

    /**
     * Validate canary and sticky configuration for one rule handle.
     *
     * @param selectorMapper selector mapper
     * @param pluginMapper plugin mapper
     * @param dictMapper dictionary mapper
     * @param selectorId target selector id
     * @param handle rule handle
     */
    public static void validateCanaryAndStickyConfig(final SelectorMapper selectorMapper,
                                                     final PluginMapper pluginMapper,
                                                     final ShenyuDictMapper dictMapper,
                                                     final String selectorId,
                                                     final String handle) {
        if (StringUtils.isBlank(handle)) {
            return;
        }
        SelectorDO selector = selectorMapper.selectById(selectorId);
        if (Objects.isNull(selector)) {
            return;
        }
        validateCanaryAndStickyConfig(pluginMapper.selectById(selector.getPluginId()), dictMapper, handle);
    }

    /**
     * Validate canary and sticky configuration against an already resolved plugin.
     *
     * @param plugin target plugin, or null when it cannot be resolved
     * @param dictMapper dictionary mapper
     * @param handle rule handle
     */
    public static void validateCanaryAndStickyConfig(final PluginDO plugin,
                                                     final ShenyuDictMapper dictMapper,
                                                     final String handle) {
        if (Objects.isNull(plugin)) {
            return;
        }
        validateCanaryAndStickyConfig(plugin.getName(), dictMapper, handle);
    }

    /**
     * Validate canary and sticky configuration against an already resolved plugin name.
     *
     * @param pluginName target plugin name
     * @param dictMapper dictionary mapper
     * @param handle rule handle
     */
    public static void validateCanaryAndStickyConfig(final String pluginName,
                                                     final ShenyuDictMapper dictMapper,
                                                     final String handle) {
        if (StringUtils.isBlank(handle) || !PluginEnum.DIVIDE.getName().equals(pluginName)) {
            return;
        }
        try {
            DivideRuleHandle divide = CanaryConfigValidator.parseHandle(handle);
            if (Objects.nonNull(divide.getCanary())) {
                CanaryConfigValidator.validateExtensions(divide.getCanary(),
                        name -> validateCanaryDictionary(dictMapper, "paramType", name),
                        name -> validateCanaryDictionary(dictMapper, "operator", name));
            }
            StickySessionConfigValidator.validate(divide.getSticky());
        } catch (IllegalArgumentException ex) {
            throw new ShenyuAdminException(ex.getMessage(), ex);
        }
    }

    /**
     * Validate the plugin-specific rule handle.
     *
     * @param selectorMapper selector mapper
     * @param pluginMapper plugin mapper
     * @param selectorId target selector id
     * @param handle rule handle
     */
    public static void validateRuleHandle(final SelectorMapper selectorMapper,
                                          final PluginMapper pluginMapper,
                                          final String selectorId,
                                          final String handle) {
        final SelectorDO selector = selectorMapper.selectById(selectorId);
        if (Objects.isNull(selector)) {
            throw new ShenyuAdminException("rule selector is not found");
        }
        validateRuleHandle(pluginMapper.selectById(selector.getPluginId()), handle);
    }

    /**
     * Validate a rule handle against an already resolved plugin.
     *
     * @param plugin target plugin
     * @param handle rule handle
     */
    public static void validateRuleHandle(final PluginDO plugin, final String handle) {
        if (Objects.isNull(plugin)) {
            throw new ShenyuAdminException("rule plugin is not found");
        }
        validateRuleHandle(plugin.getName(), handle);
    }

    /**
     * Validate a rule handle against an already resolved plugin name.
     *
     * @param pluginName target plugin name
     * @param handle rule handle
     */
    public static void validateRuleHandle(final String pluginName, final String handle) {
        if (PluginEnum.AGENT_GATEWAY.getName().equals(pluginName)) {
            AgentGatewayRuleHandleValidator.validate(handle);
        } else if (PluginEnum.REWRITE.getName().equals(pluginName)) {
            RewriteRuleHandleValidator.validate(handle);
        }
    }

    private static void validateCanaryDictionary(final ShenyuDictMapper dictMapper,
                                                 final String type,
                                                 final String name) {
        boolean supported = dictMapper.findByType(type).stream()
                .filter(dict -> Boolean.TRUE.equals(dict.getEnabled()))
                .map(ShenyuDictDO::getDictValue).anyMatch(name::equals);
        if (!supported) {
            throw new IllegalArgumentException("Unsupported Canary " + type + ": " + name);
        }
    }
}
