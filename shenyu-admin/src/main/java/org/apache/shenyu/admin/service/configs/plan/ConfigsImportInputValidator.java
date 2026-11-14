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
import org.apache.shenyu.admin.model.dto.NamespacePluginDTO;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.common.enums.SelectorTypeEnum;

import java.util.Objects;

/**
 * Shared required-field validation for namespace configuration import planning and execution.
 */
public final class ConfigsImportInputValidator {

    private ConfigsImportInputValidator() {
    }

    /**
     * Whether a plugin template cannot be imported.
     *
     * @param pluginDTO plugin template
     * @return true when required fields are missing
     */
    public static boolean invalidPlugin(final PluginDTO pluginDTO) {
        return Objects.isNull(pluginDTO)
                || StringUtils.isBlank(pluginDTO.getRole())
                || Objects.isNull(pluginDTO.getEnabled());
    }

    /**
     * Whether a namespace plugin relation cannot be imported.
     *
     * @param pluginDTO namespace plugin relation
     * @return true when required fields are missing
     */
    public static boolean invalidNamespacePlugin(final NamespacePluginDTO pluginDTO) {
        return Objects.isNull(pluginDTO) || Objects.isNull(pluginDTO.getEnabled());
    }

    /**
     * Whether a selector cannot be imported.
     *
     * @param selectorDTO selector
     * @return true when required fields are missing
     */
    public static boolean invalidSelector(final SelectorDTO selectorDTO) {
        if (Objects.isNull(selectorDTO) || Objects.isNull(selectorDTO.getType()) || Objects.isNull(selectorDTO.getSort())
                || Objects.isNull(selectorDTO.getEnabled()) || Objects.isNull(selectorDTO.getLogged())
                || Objects.isNull(selectorDTO.getContinued()) || Objects.isNull(selectorDTO.getMatchRestful())
                || Objects.isNull(selectorDTO.getSelectorConditions())) {
            return true;
        }
        return !Objects.equals(SelectorTypeEnum.FULL_FLOW.getCode(), selectorDTO.getType())
                && Objects.isNull(selectorDTO.getMatchMode());
    }

    /**
     * Whether a rule cannot be imported.
     *
     * @param ruleDTO rule
     * @return true when required fields are missing
     */
    public static boolean invalidRule(final RuleDTO ruleDTO) {
        return Objects.isNull(ruleDTO) || Objects.isNull(ruleDTO.getMatchMode()) || Objects.isNull(ruleDTO.getSort())
                || Objects.isNull(ruleDTO.getEnabled()) || Objects.isNull(ruleDTO.getLogged())
                || Objects.isNull(ruleDTO.getMatchRestful()) || Objects.isNull(ruleDTO.getRuleConditions());
    }

    /**
     * Reject an invalid plugin template before import writes.
     *
     * @param pluginDTO plugin template
     */
    public static void requireValidPlugin(final PluginDTO pluginDTO) {
        if (Objects.isNull(pluginDTO) || StringUtils.isBlank(pluginDTO.getName()) || invalidPlugin(pluginDTO)) {
            throw new ShenyuAdminException("invalid plugin: " + name(Objects.isNull(pluginDTO) ? null : pluginDTO.getName()));
        }
    }

    /**
     * Reject an invalid namespace plugin relation before import writes.
     *
     * @param pluginDTO namespace plugin relation
     */
    public static void requireValidNamespacePlugin(final NamespacePluginDTO pluginDTO) {
        if (Objects.isNull(pluginDTO) || StringUtils.isBlank(pluginDTO.getPluginId()) || invalidNamespacePlugin(pluginDTO)) {
            throw new ShenyuAdminException("invalid namespace plugin: "
                    + name(Objects.isNull(pluginDTO) ? null : pluginDTO.getName()));
        }
    }

    /**
     * Reject an invalid selector before import writes.
     *
     * @param selectorDTO selector
     */
    public static void requireValidSelector(final SelectorDTO selectorDTO) {
        if (Objects.isNull(selectorDTO) || StringUtils.isBlank(selectorDTO.getName()) || invalidSelector(selectorDTO)) {
            throw new ShenyuAdminException("invalid selector: " + name(Objects.isNull(selectorDTO) ? null : selectorDTO.getName()));
        }
    }

    /**
     * Reject an invalid rule before import writes.
     *
     * @param ruleDTO rule
     */
    public static void requireValidRule(final RuleDTO ruleDTO) {
        if (Objects.isNull(ruleDTO) || StringUtils.isBlank(ruleDTO.getName())
                || StringUtils.isBlank(ruleDTO.getSelectorId()) || invalidRule(ruleDTO)) {
            throw new ShenyuAdminException("invalid rule: " + name(Objects.isNull(ruleDTO) ? null : ruleDTO.getName()));
        }
    }

    private static String name(final String value) {
        return StringUtils.defaultString(value);
    }
}
