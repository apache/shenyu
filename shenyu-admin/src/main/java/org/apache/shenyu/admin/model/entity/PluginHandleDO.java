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
import org.apache.shenyu.admin.model.dto.PluginHandleDTO;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.util.Optional;

/**
 * plugin handle json definition.
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class PluginHandleDO extends BaseDO {

    private static final long serialVersionUID = 3854807942396454551L;

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
     * 3 indicates select box.
     */
    private Integer dataType;

    /**
     *  the attribute type.
     *  1  selector,
     *  2  rule.
     */
    private Integer type;

    /**
     * the attribute sort.
     */
    private Integer sort;

    /**
     * the attribute extObj.
     */
    private String extObj;

    /**
     * build {@linkplain PluginHandleDO} instance.
     * @param pluginHandleDTO {@linkplain PluginHandleDTO}
     * @return {@linkplain PluginHandleDO}
     */
    public static PluginHandleDO buildPluginHandleDO(final PluginHandleDTO pluginHandleDTO) {
        return Optional.ofNullable(pluginHandleDTO).map(item -> {
            PluginHandleDO pluginHandleDO = PluginHandleDO.builder()
                    .id(item.getId())
                    .pluginId(item.getPluginId())
                    .field(item.getField())
                    .label(item.getLabel())
                    .dataType(item.getDataType())
                    .type(item.getType())
                    .sort(item.getSort())
                    .extObj(item.getExtObj())
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                pluginHandleDO.setId(UUIDUtils.getInstance().generateShortUuid());
            }
            return pluginHandleDO;
        }).orElse(null);
    }
}
