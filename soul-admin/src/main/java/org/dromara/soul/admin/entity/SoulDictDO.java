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

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.SoulDictDTO;
import org.dromara.soul.common.utils.UUIDUtils;

import java.util.Objects;

/**
 * SoulDictDO.
 *
 * @author dengliming
 */
@Data
public class SoulDictDO extends BaseDO {

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
        if (Objects.isNull(soulDictDTO)) {
            return null;
        }

        SoulDictDO soulDictDO = new SoulDictDO();
        soulDictDO.setId(soulDictDTO.getId());
        soulDictDO.setDictCode(soulDictDTO.getDictCode());
        soulDictDO.setDictName(soulDictDTO.getDictName());
        soulDictDO.setDictValue(soulDictDTO.getDictValue());
        soulDictDO.setDesc(soulDictDTO.getDesc());
        soulDictDO.setEnabled(soulDictDTO.getEnabled());
        soulDictDO.setSort(soulDictDTO.getSort());
        soulDictDO.setType(soulDictDTO.getType());
        if (StringUtils.isEmpty(soulDictDTO.getId())) {
            soulDictDO.setId(UUIDUtils.getInstance().generateShortUuid());
        }
        return soulDictDO;
    }
}
