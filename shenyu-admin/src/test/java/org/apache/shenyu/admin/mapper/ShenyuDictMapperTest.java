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

package org.apache.shenyu.admin.mapper;

import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.model.query.ShenyuDictQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.Test;
import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.List;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.Assert.assertThat;

/**
 * Test cases for ShenyuDictMapper.
 */
public final class ShenyuDictMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private ShenyuDictMapper shenyuDictMapper;

    @Test
    public void testSelectByQuery() {
        ShenyuDictDO record = buildShenyuDictDO();
        int count = shenyuDictMapper.insert(record);
        assertThat(count, greaterThan(0));

        ShenyuDictQuery shenyuDictQuery = new ShenyuDictQuery();
        List<ShenyuDictDO> shenyuDictList = shenyuDictMapper.selectByQuery(shenyuDictQuery);
        assertThat(shenyuDictList.size(), greaterThan(0));
    }

    @Test
    public void testInsertAndUpdate() {
        ShenyuDictDO record = buildShenyuDictDO();
        int count = shenyuDictMapper.insert(record);
        assertThat(count, greaterThan(0));

        record.setDesc("test1");
        count = shenyuDictMapper.updateByPrimaryKey(record);
        assertThat(count, greaterThan(0));

        count = shenyuDictMapper.delete(record.getId());
        assertThat(count, greaterThan(0));
    }

    private ShenyuDictDO buildShenyuDictDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        return ShenyuDictDO.builder()
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
