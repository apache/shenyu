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

package org.apache.shenyu.plugin.casdoor.handle;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.casbin.casdoor.config.CasdoorConfig;
import org.casbin.casdoor.service.CasdoorAuthService;

import java.util.Map;
import java.util.Optional;

public class CasdoorPluginDateHandler implements PluginDataHandler {
    @Override
    public void handlerPlugin(final PluginData pluginData) {
        Map<String, String> configMap = GsonUtils.getInstance().toObjectMap(pluginData.getConfig(), String.class);
        final String endpoint = Optional.ofNullable(configMap.get("endpoint")).orElse("");
        final String clientSecrect = Optional.ofNullable(configMap.get("client_secrect")).orElse("");
        final String clientId = Optional.ofNullable(configMap.get("client_id")).orElse("");
        String certificate = Optional.ofNullable(configMap.get("certificate")).orElse("");
        certificate = certificate.replace("\\n", "\n");
        String organization = Optional.ofNullable(configMap.get("organization-name")).orElse("");
        String application = Optional.ofNullable(configMap.get("application-name")).orElse("");
        CasdoorConfig casdoorConfig = new CasdoorConfig(endpoint, clientId, clientSecrect, certificate, organization, application);
        CasdoorAuthService casdoorAuthService = new CasdoorAuthService(casdoorConfig);
        Singleton.INST.single(CasdoorAuthService.class, casdoorAuthService);
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.CASDOOR.getName();
    }
}
