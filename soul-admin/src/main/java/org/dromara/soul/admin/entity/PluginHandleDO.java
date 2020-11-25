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

package org.dromara.soul.admin.entity;

import java.util.Objects;
import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.PluginHandleDTO;
import org.dromara.soul.common.utils.UUIDUtils;

/**
 * plugin handle json definition.
 * @author liangziqiang
 */
@Data
public class PluginHandleDO extends BaseDO {

    /**
     * plugin id.
     */
    private String pluginId;

    /**
     * the attribute name.
     */
    private String field;

    /**
     * the attribute label.
     */
    private String label;

    /**
     * the data type.
     * 1 indicates number
     * 2 indicates string
     * 3 indicates select box
     */
    private Integer dataType;

    /**
     * build {@linkplain PluginHandleDO} instance.
     * @param pluginHandleDTO {@linkplain PluginHandleDTO}
     * @return {@linkplain PluginHandleDO}
     */
    public static PluginHandleDO buildPluginHandleDO(final PluginHandleDTO pluginHandleDTO) {
        if (Objects.isNull(pluginHandleDTO)) {
            return null;
        }

        PluginHandleDO pluginHandleDO = new PluginHandleDO();
        pluginHandleDO.setId(pluginHandleDTO.getId());
        pluginHandleDO.setPluginId(pluginHandleDTO.getPluginId());
        pluginHandleDO.setField(pluginHandleDTO.getField());
        pluginHandleDO.setLabel(pluginHandleDTO.getLabel());
        pluginHandleDO.setDataType(pluginHandleDTO.getDataType());
        if (StringUtils.isEmpty(pluginHandleDTO.getId())) {
            pluginHandleDO.setId(UUIDUtils.getInstance().generateShortUuid());
        }
        return pluginHandleDO;
    }
}
