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
import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.util.Optional;

/**
 * ShenyuDictDO.
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class ShenyuDictDO extends BaseDO {

    private static final long serialVersionUID = -3968123108441795604L;

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
     * build {@linkplain ShenyuDictDO} instance.
     *
     * @param shenyuDictDTO {@linkplain ShenyuDictDTO}
     * @return {@linkplain ShenyuDictDO}
     */
    public static ShenyuDictDO buildShenyuDictDO(final ShenyuDictDTO shenyuDictDTO) {
        return Optional.ofNullable(shenyuDictDTO).map(item -> {
            ShenyuDictDO shenyuDictDO = ShenyuDictDO.builder()
                    .id(item.getId())
                    .dictCode(item.getDictCode())
                    .dictName(item.getDictName())
                    .dictValue(item.getDictValue())
                    .desc(item.getDesc())
                    .enabled(item.getEnabled())
                    .sort(item.getSort())
                    .type(item.getType())
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                shenyuDictDO.setId(UUIDUtils.getInstance().generateShortUuid());
            }
            return shenyuDictDO;
        }).orElse(null);
    }
}
