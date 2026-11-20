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

package org.apache.shenyu.admin.validation.validator;

import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Objects;

/**
 * Validates HTTP registrations in Admin while keeping the wire models JDK-only.
 */
public final class ShenyuClientRegistryValidator implements Validator {

    @Override
    public boolean supports(final Class<?> clazz) {
        return MetaDataRegisterDTO.class.isAssignableFrom(clazz)
                || URIRegisterDTO.class.isAssignableFrom(clazz)
                || ApiDocRegisterDTO.class.isAssignableFrom(clazz)
                || DiscoveryConfigRegisterDTO.class.isAssignableFrom(clazz)
                || McpToolsRegisterDTO.class.isAssignableFrom(clazz);
    }

    @Override
    public void validate(final Object target, final Errors errors) {
        if (target instanceof MetaDataRegisterDTO) {
            validateMetadata((MetaDataRegisterDTO) target, errors, "");
        } else if (target instanceof URIRegisterDTO) {
            validateUri((URIRegisterDTO) target, errors);
        } else if (target instanceof ApiDocRegisterDTO) {
            validateApiDoc((ApiDocRegisterDTO) target, errors);
        } else if (target instanceof DiscoveryConfigRegisterDTO) {
            validateDiscoveryConfig((DiscoveryConfigRegisterDTO) target, errors);
        } else if (target instanceof McpToolsRegisterDTO) {
            validateMcpTools((McpToolsRegisterDTO) target, errors);
        }
    }

    private void validateMetadata(final MetaDataRegisterDTO target, final Errors errors, final String prefix) {
        rejectIfBlank(errors, prefix + "appName", target.getAppName());
        rejectIfBlank(errors, prefix + "path", target.getPath());
        rejectIfBlank(errors, prefix + "rpcType", target.getRpcType());
        rejectIfPresentBlank(errors, prefix + "contextPath", target.getContextPath());
        rejectIfPresentBlank(errors, prefix + "ruleName", target.getRuleName());
        rejectIfPresentBlank(errors, prefix + "host", target.getHost());
    }

    private void validateUri(final URIRegisterDTO target, final Errors errors) {
        rejectIfBlank(errors, "appName", target.getAppName());
        rejectIfBlank(errors, "rpcType", target.getRpcType());
        rejectIfPresentBlank(errors, "contextPath", target.getContextPath());
        rejectIfPresentBlank(errors, "host", target.getHost());
    }

    private void validateApiDoc(final ApiDocRegisterDTO target, final Errors errors) {
        rejectIfBlank(errors, "contextPath", target.getContextPath());
        rejectIfBlank(errors, "apiPath", target.getApiPath());
        rejectIfBlank(errors, "rpcType", target.getRpcType());
        rejectIfNull(errors, "httpMethod", target.getHttpMethod());
        rejectIfNull(errors, "eventType", target.getEventType());
    }

    private void validateDiscoveryConfig(final DiscoveryConfigRegisterDTO target, final Errors errors) {
        rejectIfBlank(errors, "selectorName", target.getSelectorName());
        rejectIfBlank(errors, "name", target.getName());
        rejectIfBlank(errors, "discoveryType", target.getDiscoveryType());
        rejectIfBlank(errors, "serverList", target.getServerList());
        rejectIfBlank(errors, "pluginName", target.getPluginName());
    }

    private void validateMcpTools(final McpToolsRegisterDTO target, final Errors errors) {
        rejectIfNull(errors, "metaDataRegisterDTO", target.getMetaDataRegisterDTO());
        rejectIfBlank(errors, "mcpConfig", target.getMcpConfig());
        if (Objects.nonNull(target.getMetaDataRegisterDTO())) {
            validateMetadata(target.getMetaDataRegisterDTO(), errors, "metaDataRegisterDTO.");
        }
    }

    private void rejectIfBlank(final Errors errors, final String field, final String value) {
        if (Objects.isNull(value) || value.chars().allMatch(Character::isWhitespace)) {
            errors.rejectValue(field, "NotBlank", "must not be blank");
        }
    }

    private void rejectIfPresentBlank(final Errors errors, final String field, final String value) {
        if (Objects.nonNull(value) && !value.isEmpty() && value.chars().allMatch(Character::isWhitespace)) {
            errors.rejectValue(field, "NotBlank", "must not be blank");
        }
    }

    private void rejectIfNull(final Errors errors, final String field, final Object value) {
        if (Objects.isNull(value)) {
            errors.rejectValue(field, "NotNull", "must not be null");
        }
    }
}
