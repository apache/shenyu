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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginEnum;

/**
 * Validates a selector's AI Proxy mode before publishing it to gateways.
 */
public final class AiProxySelectorValidation {

    private AiProxySelectorValidation() {
    }

    /**
     * Reject unsupported modes and unusable native routing configuration.
     *
     * @param pluginName plugin owning this selector
     * @param serializedHandle selector handle JSON
     */
    public static void validateHandle(final String pluginName, final String serializedHandle) {
        if (!PluginEnum.AI_PROXY.getName().equals(pluginName) || StringUtils.isEmpty(serializedHandle)) {
            return;
        }
        try {
            AiProxyHandle.fromSelectorHandle(serializedHandle);
        } catch (IllegalArgumentException e) {
            throw new ShenyuAdminException("Invalid AI Proxy selector: " + e.getMessage());
        }
    }
}
