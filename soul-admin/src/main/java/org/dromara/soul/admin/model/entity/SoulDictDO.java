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

package org.dromara.soul.admin.model.entity;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.model.dto.SoulDictDTO;
import org.dromara.soul.common.utils.UUIDUtils;

import java.util.Optional;

/**
 * SoulDictDO.
 *
 * @author dengliming
 * @author nuo-promise
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class SoulDictDO extends BaseDO {

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
     * build {@linkplain SoulDictDO} instance.
     *
     * @param soulDictDTO {@linkplain SoulDictDTO}
     * @return {@linkplain SoulDictDO}
     */
    public static SoulDictDO buildSoulDictDO(final SoulDictDTO soulDictDTO) {
        return Optional.ofNullable(soulDictDTO).map(item -> {
            SoulDictDO soulDictDO = SoulDictDO.builder()
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
                soulDictDO.setId(UUIDUtils.getInstance().generateShortUuid());
            }
            return soulDictDO;
        }).orElse(null);
    }
}
