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
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;

import java.io.Serializable;
import java.util.Optional;

/**
 * data permission page list vo.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class DataPermissionPageVO implements Serializable {

    private static final long serialVersionUID = -7532270386821533624L;

    /**
     * selector id or rule id.
     */
    private String dataId;

    /**
     * selector name or rule name.
     */
    private String dataName;

    /**
     * whether checked.
     */
    private Boolean isChecked;


    /**
     * build vo by selector.
     * @param selectorDO {@linkplain SelectorDO}
     * @param isChecked whether checked
     * @return {@linkplain DataPermissionPageVO}
     */
    public static DataPermissionPageVO buildPageVOBySelector(final SelectorDO selectorDO, final Boolean isChecked) {
        return Optional.ofNullable(selectorDO)
                .map(item -> new DataPermissionPageVO(item.getId(), item.getName(), isChecked))
                .orElse(null);
    }

    /**
     * build data permission page vo by rule do.
     * @param ruleDO {@linkplain RuleDO}
     * @param isChecked whether checked
     * @return  {@linkplain DataPermissionPageVO}
     */
    public static DataPermissionPageVO buildPageVOByRule(final RuleDO ruleDO, final Boolean isChecked) {
        return Optional.of(ruleDO)
                .map(item -> new DataPermissionPageVO(item.getId(), item.getName(), isChecked))
                .orElse(null);
    }
}
