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

package org.dromara.soul.admin.mapper;

import org.dromara.soul.admin.AbstractSpringIntegrationTest;
import org.dromara.soul.admin.entity.SoulDictDO;
import org.dromara.soul.admin.query.SoulDictQuery;
import org.dromara.soul.common.utils.UUIDUtils;
import org.junit.Test;
import javax.annotation.Resource;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.List;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.Assert.assertThat;

/**
 * Test cases for SoulDictMapper.
 *
 * @author dengliming
 */
public final class SoulDictMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private SoulDictMapper soulDictMapper;

    @Test
    public void testSelectByQuery() {
        SoulDictDO record = buildSoulDictDO();
        int count = soulDictMapper.insert(record);
        assertThat(count, greaterThan(0));

        SoulDictQuery soulDictQuery = new SoulDictQuery();
        List<SoulDictDO> soulDictList = soulDictMapper.selectByQuery(soulDictQuery);
        assertThat(soulDictList.size(), greaterThan(0));
    }

    @Test
    public void testInsertAndUpdate() {
        SoulDictDO record = buildSoulDictDO();
        int count = soulDictMapper.insert(record);
        assertThat(count, greaterThan(0));

        record.setDesc("test1");
        count = soulDictMapper.updateByPrimaryKey(record);
        assertThat(count, greaterThan(0));

        count = soulDictMapper.delete(record.getId());
        assertThat(count, greaterThan(0));
    }

    private SoulDictDO buildSoulDictDO() {
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        String id = UUIDUtils.getInstance().generateShortUuid();
        return SoulDictDO.builder()
                .id(id)
                .sort(1)
                .desc("test")
                .dictCode("t_dict_1")
                .dictName("t_d_v")
                .enabled(false)
                .type("rule")
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }
}
