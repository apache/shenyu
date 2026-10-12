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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.mapper.PluginHandleMapper;
import org.apache.shenyu.admin.mapper.ShenyuDictMapper;
import org.apache.shenyu.admin.service.publish.PluginHandleEventPublisher;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link PluginHandleServiceImpl}.
 */
@ExtendWith(MockitoExtension.class)
public final class PluginHandleServiceImplTest {

    @Mock
    private PluginHandleMapper pluginHandleMapper;

    @Mock
    private ShenyuDictMapper shenyuDictMapper;

    @Mock
    private PluginHandleEventPublisher eventPublisher;

    @Test
    public void findByIdShouldNotThrowForUnknownId() {
        PluginHandleServiceImpl service = new PluginHandleServiceImpl(pluginHandleMapper, shenyuDictMapper, eventPublisher);
        when(pluginHandleMapper.selectById("missing")).thenReturn(null);
        assertNull(service.findById("missing"));
    }
}
