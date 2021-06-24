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

package org.apache.shenyu.admin.model.entity;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.Optional;

/**
 * The config field has been added in 2.0
 * PluginDO.
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class PluginDO extends BaseDO {

    private static final long serialVersionUID = -3414676617520629553L;

    /**
     * plugin name.
     */
    private String name;

    /**
     * plugin config @see 2.0.
     */
    private String config;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * the role to classify plugin.
     */
    private String role;

    /**
     * plugin sort.
     */
    private Integer sort;

    /**
     * build pluginDO.
     *
     * @param pluginDTO {@linkplain PluginDTO}
     * @return {@linkplain PluginDO}
     */
    public static PluginDO buildPluginDO(final PluginDTO pluginDTO) {
        return Optional.ofNullable(pluginDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            PluginDO pluginDO = PluginDO.builder()
                    .name(item.getName())
                    .config(item.getConfig())
                    .enabled(item.getEnabled())
                    .role(item.getRole())
                    .sort(item.getSort())
                    .dateUpdated(currentTime)
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                pluginDO.setId(UUIDUtils.getInstance().generateShortUuid());
                pluginDO.setDateCreated(currentTime);
            } else {
                pluginDO.setId(item.getId());
            }
            return pluginDO;
        }).orElse(null);
    }
}
