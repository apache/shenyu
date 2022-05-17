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

package org.apache.shenyu.plugin.base.utils;

import org.apache.shenyu.plugin.api.result.DefaultShenyuEntity;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ConfigurableApplicationContext;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Shenyu result wrap test.
 */
@ExtendWith(MockitoExtension.class)
public final class ShenyuResultWrapTest {

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
    }

    /**
     * The success test.
     */
    @Test
    public void successTest() {
        Integer result = 0;
        final DefaultShenyuEntity success = new DefaultShenyuEntity(result, "success", null);
        DefaultShenyuEntity shenyuResult = (DefaultShenyuEntity) ShenyuResultWrap.success(null, success);
        assertEquals(shenyuResult.getCode(), result);
        assertEquals(shenyuResult.getMessage(), "success");
    }

    /**
     * The error test.
     */
    @Test
    public void errorTest() {
        Integer result = 1;
        DefaultShenyuEntity shenyuResult = (DefaultShenyuEntity) ShenyuResultWrap.error(null, result, "error", new Object());
        assertEquals(result, shenyuResult.getCode());
        assertEquals("error", shenyuResult.getMessage());
    }
}
