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

package org.apache.shenyu.admin.transfer;

import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.common.dto.PluginData;
import org.springframework.util.Base64Utils;

import java.util.Optional;

/**
 * The interface Plugin transfer.
 */
public enum PluginTransfer {

    /**
     * The constant INSTANCE.
     */
    INSTANCE;

    /**
     * Map to data plugin data.
     *
     * @param pluginDO the plugin do
     * @return the plugin data
     */
    public PluginData mapToData(final PluginDO pluginDO) {
        return Optional.ofNullable(pluginDO)
                .map(v -> PluginData.builder()
                        .id(v.getId())
                        .name(v.getName())
                        .config(v.getConfig())
                        .role(v.getRole())
                        .enabled(v.getEnabled())
                        .sort(v.getSort())
                        .pluginJar(Optional.ofNullable(v.getPluginJar()).map(Base64Utils::encodeToString).orElse(""))
                        .build())
                .orElse(null);
    }

    /**
     * Map data tovo plugin data.
     *
     * @param pluginVO the plugin vo
     * @return the plugin data
     */
    public PluginData mapDataTOVO(final PluginVO pluginVO) {
        return Optional.ofNullable(pluginVO)
                .map(v -> PluginData.builder()
                        .id(v.getId())
                        .name(v.getName())
                        .config(v.getConfig())
                        .role(v.getRole())
                        .enabled(v.getEnabled())
                        .sort(v.getSort())
                        .build())
                .orElse(null);
    }
}
