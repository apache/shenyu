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
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.Optional;

/**
 * this is shenyu dict view to web front.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ShenyuDictVO implements Serializable {

    private static final long serialVersionUID = 5731120468713362319L;

    /**
     * primary key.
     */
    private String id;

    /**
     * dict type.
     */
    private String type;

    /**
     * dict code.
     */
    private String dictCode;

    /**
     * dict name.
     */
    private String dictName;

    /**
     * dict value.
     */
    private String dictValue;

    /**
     * dict desc.
     */
    private String desc;

    /**
     * sort no.
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
     * build {@linkplain ShenyuDictVO}.
     *
     * @param shenyuDictDO {@linkplain ShenyuDictDO}
     * @return {@linkplain ShenyuDictVO}
     */
    public static ShenyuDictVO buildShenyuDictVO(final ShenyuDictDO shenyuDictDO) {
        return Optional.ofNullable(shenyuDictDO)
                .map(it -> new ShenyuDictVO(shenyuDictDO.getId(), shenyuDictDO.getType(),
                        shenyuDictDO.getDictCode(), shenyuDictDO.getDictName(),
                        shenyuDictDO.getDictValue(), shenyuDictDO.getDesc(), shenyuDictDO.getSort(), shenyuDictDO.getEnabled(),
                        DateUtils.localDateTimeToString(shenyuDictDO.getDateCreated().toLocalDateTime()),
                        DateUtils.localDateTimeToString(shenyuDictDO.getDateUpdated().toLocalDateTime())))
                .orElse(null);
    }
}
