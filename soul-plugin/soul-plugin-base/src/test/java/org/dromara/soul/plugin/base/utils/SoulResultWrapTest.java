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

package org.dromara.soul.plugin.base.utils;

import org.dromara.soul.plugin.api.result.DefaultSoulEntity;
import org.dromara.soul.plugin.api.result.DefaultSoulResult;
import org.dromara.soul.plugin.api.result.SoulResult;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Soul result wrap test.
 *
 * @author zhanglei
 */
@RunWith(MockitoJUnitRunner.class)
public final class SoulResultWrapTest {

    @Before
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setCfgContext(context);
        when(context.getBean(SoulResult.class)).thenReturn(new DefaultSoulResult());
    }

    /**
     * The success test.
     */
    @Test
    public void successTest() {
        Integer result = 0;
        DefaultSoulEntity soulResult = (DefaultSoulEntity) SoulResultWrap.success(0, "success", new Object());
        Assert.assertEquals(soulResult.getCode(), result);
        Assert.assertEquals(soulResult.getMessage(), "success");
    }

    /**
     * The error test.
     */
    @Test
    public void errorTest() {
        Integer result = 1;
        DefaultSoulEntity soulResult = (DefaultSoulEntity) SoulResultWrap.error(1, "error", new Object());
        Assert.assertEquals(soulResult.getCode(), result);
        Assert.assertEquals(soulResult.getMessage(), "error");
    }
}
