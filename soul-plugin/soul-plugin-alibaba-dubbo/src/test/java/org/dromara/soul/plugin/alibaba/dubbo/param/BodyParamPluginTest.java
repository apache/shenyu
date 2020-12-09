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
package org.dromara.soul.plugin.alibaba.dubbo.param;

import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

/**
 * Test case for {@link BodyParamPlugin}.
 *
 * @author Phoenix Luo
 **/

@RunWith(MockitoJUnitRunner.class)
public final class BodyParamPluginTest {
    private SoulPluginChain chain;
    
    private BodyParamPlugin bodyParamPlugin;
    
    @Before
    public void setUp() {
        bodyParamPlugin = new BodyParamPlugin();
        chain = mock(SoulPluginChain.class);
    }
    
    @Test
    public void testGetOrder() {
        final int result = bodyParamPlugin.getOrder();
        assertEquals(PluginEnum.DUBBO.getCode() - 1, result);
    }
    
    @Test
    public void testNamed() {
        final String result = bodyParamPlugin.named();
        assertEquals("alibaba-dubbo-body-param", result);
    }
    
}
