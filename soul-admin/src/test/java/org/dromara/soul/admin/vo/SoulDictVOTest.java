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

package org.dromara.soul.admin.vo;

import org.dromara.soul.admin.entity.SoulDictDO;
import org.dromara.soul.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;
import java.time.LocalDateTime;

/**
 * Test case for SoulDictVO.
 *
 * @author midnight2104
 */
public class SoulDictVOTest {

    /**
     * test getter and setter method of SoulDictVO.
     */
    @Test
    public void shouldSuccessGetAndSetMethod() {
        SoulDictVO soulDictVO = getSoulDictVO();

        assert soulDictVO != null;

        soulDictVO.setId(soulDictVO.getId());
        soulDictVO.setSort(soulDictVO.getSort());
        soulDictVO.setDesc(soulDictVO.getDesc());
        soulDictVO.setDictCode(soulDictVO.getDictCode());
        soulDictVO.setDictName(soulDictVO.getDictName());
        soulDictVO.setEnabled(soulDictVO.getEnabled());
        soulDictVO.setType(soulDictVO.getType());
        soulDictVO.setDateCreated(soulDictVO.getDateUpdated());
        soulDictVO.setDateUpdated(soulDictVO.getDateUpdated());
    }

    /**
     * get SoulDictVO.
     *
     * @return soulDictVO object
     */
    private SoulDictVO getSoulDictVO() {
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        String id = UUIDUtils.getInstance().generateShortUuid();

        return SoulDictVO.buildSoulDictVO(SoulDictDO.builder()
                .id(id)
                .sort(1)
                .desc("test")
                .dictCode("t_dict_1")
                .dictName("t_d_v")
                .enabled(false)
                .type("rule")
                .dateCreated(now)
                .dateUpdated(now)
                .build());
    }
}
