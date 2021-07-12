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

package org.apache.shenyu.admin.model.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;

/**
 * this is plugin view to web front.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class PluginVO implements Serializable {

    private static final long serialVersionUID = 7537793180460522887L;

    /**
     * primary key.
     */
    private String id;

    /**
     * plugin role.
     */
    private String role;

    /**
     * plugin name.
     */
    private String name;

    /**
     * plugin config.
     */
    private String config;

    /**
     * plugin sort.
     */
    private Integer sort;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    /**
     * build pluginVO.
     *
     * @param pluginDO {@linkplain PluginDO}
     * @return {@linkplain PluginVO}
     */
    public static PluginVO buildPluginVO(final PluginDO pluginDO) {
        return new PluginVO(pluginDO.getId(), pluginDO.getRole(), pluginDO.getName(),
                pluginDO.getConfig(), pluginDO.getSort(), pluginDO.getEnabled(),
                DateUtils.localDateTimeToString(pluginDO.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(pluginDO.getDateUpdated().toLocalDateTime()));
    }
}
