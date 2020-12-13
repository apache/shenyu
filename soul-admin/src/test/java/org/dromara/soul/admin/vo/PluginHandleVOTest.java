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

import org.dromara.soul.common.utils.DateUtils;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.util.ArrayList;

/**
 * Test case for PluginHandleVO.
 *
 * @author midnight2104
 */
public class PluginHandleVOTest {

    /**
     * test getter and setter method of PluginHandleVO.
     */
    @Test
    public void shouldSuccessGetAndSetMethod() {
        PluginHandleVO pluginHandleVO = new PluginHandleVO("1", "2", "3", "label",
                1, 1, 1, null, DateUtils.localDateTimeToString(LocalDateTime.now()),
                DateUtils.localDateTimeToString(LocalDateTime.now()), new ArrayList<>());

        pluginHandleVO.setId(pluginHandleVO.getId());
        pluginHandleVO.setPluginId(pluginHandleVO.getPluginId());
        pluginHandleVO.setField(pluginHandleVO.getField());
        pluginHandleVO.setLabel(pluginHandleVO.getLabel());
        pluginHandleVO.setDataType(pluginHandleVO.getDataType());
        pluginHandleVO.setType(pluginHandleVO.getType());
        pluginHandleVO.setSort(pluginHandleVO.getSort());
        pluginHandleVO.setExtObj(pluginHandleVO.getExtObj());
        pluginHandleVO.setDateCreated(pluginHandleVO.getDateCreated());
        pluginHandleVO.setDateUpdated(pluginHandleVO.getDateUpdated());
        pluginHandleVO.setDictOptions(pluginHandleVO.getDictOptions());
    }
}
