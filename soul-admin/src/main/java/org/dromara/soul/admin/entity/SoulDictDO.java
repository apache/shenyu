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

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.SoulDictDTO;
import org.dromara.soul.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.Optional;

/**
 * SoulDictDO.
 *
 * @author dengliming
 * @author nuo-promise
 */
@Data
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

    @Builder
    private SoulDictDO(final String id, final Timestamp dateCreated, final Timestamp dateUpdated, final String type,
                       final String dictCode, final String dictName, final String dictValue, final String desc,
                       final Integer sort, final Boolean enabled) {
        super(id, dateCreated, dateUpdated);
        this.type = type;
        this.dictCode = dictCode;
        this.dictName = dictName;
        this.dictValue = dictValue;
        this.desc = desc;
        this.sort = sort;
        this.enabled = enabled;
    }

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
