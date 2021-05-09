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
import org.apache.shenyu.admin.model.entity.PluginHandleDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

/**
 * this is plugin handle view to web front.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class PluginHandleVO implements Serializable {

    private static final long serialVersionUID = 940877592520676748L;

    /**
     * primary key.
     */
    private String id;

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
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    private List<ShenyuDictVO> dictOptions;

    /**
     * build {@linkplain PluginHandleVO}.
     *
     * @param pluginHandleDO {@linkplain PluginHandleDO}
     * @param dictOptions dictOptions
     * @return {@linkplain PluginHandleVO}
     */
    public static PluginHandleVO buildPluginHandleVO(final PluginHandleDO pluginHandleDO, final List<ShenyuDictVO> dictOptions) {
        return Optional.ofNullable(pluginHandleDO)
                .map(it -> new PluginHandleVO(pluginHandleDO.getId(), pluginHandleDO.getPluginId(),
                        pluginHandleDO.getField(), pluginHandleDO.getLabel(),
                        pluginHandleDO.getDataType(), pluginHandleDO.getType(), pluginHandleDO.getSort(), pluginHandleDO.getExtObj(),
                        DateUtils.localDateTimeToString(pluginHandleDO.getDateCreated().toLocalDateTime()),
                        DateUtils.localDateTimeToString(pluginHandleDO.getDateUpdated().toLocalDateTime()), dictOptions))
                .orElse(null);
    }
}
