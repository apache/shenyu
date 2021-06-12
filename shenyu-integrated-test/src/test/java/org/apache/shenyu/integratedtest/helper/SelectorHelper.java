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
package org.apache.shenyu.integratedtest.helper;

import com.google.common.collect.Lists;
import com.google.gson.reflect.TypeToken;
import org.apache.shenyu.integratedtest.dto.AdminResponse;
import org.apache.shenyu.integratedtest.dto.SelectorConditionDTO;
import org.apache.shenyu.integratedtest.dto.SelectorDTO;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class SelectorHelper {

    private final AtomicInteger sort = new AtomicInteger(0);

    public AdminResponse<List<SelectorDTO>> getAllSelectors(int pluginId) throws Exception {
        // 100 is big enough, just for test
        return HttpHelper.INSTANCE.getFromAdmin(
                String.format("/selector?pluginId=%d&currentPage=1&pageSize=100", pluginId),
                new TypeToken<List<SelectorDTO>>() {
                }.getType());
    }

    public AdminResponse<Object> createSelector(int pluginId, String name, String uri) throws Exception {
        SelectorDTO selectorDTO = SelectorDTO.builder()
                .name(name)
                .sort(sort.incrementAndGet())
                .continued(true)
                .enabled(true)
                .selectorConditions(Lists.newArrayList(SelectorConditionDTO.builder()
                        .operator("=")
                        .paramName("/")
                        .paramType("uri")
                        .paramValue(uri)
                        .build()))
                .matchMode(0)
                .pluginId(pluginId + "")
                .type(1)
                .loged(true)
                .build();
        return HttpHelper.INSTANCE.postAdmin("/selector", selectorDTO, (new TypeToken<AdminResponse<Object>>() {
        }).getType());
    }
}
