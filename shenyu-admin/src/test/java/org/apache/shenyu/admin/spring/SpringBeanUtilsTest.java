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

package org.apache.shenyu.admin.spring;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link SpringBeanUtils}.
 */
public final class SpringBeanUtilsTest {

    private SpringBeanUtils springBeanUtilsUnderTest;

    @Before
    public void setUp() {
        springBeanUtilsUnderTest = SpringBeanUtils.getInstance();
    }

    @Test
    public void testGetBean() {
        ConfigurableApplicationContext applicationContext = mock(ConfigurableApplicationContext.class);
        when(applicationContext.getBean(TestBean.class)).thenReturn(new TestBean());
        springBeanUtilsUnderTest.setApplicationContext(applicationContext);
        final TestBean result = springBeanUtilsUnderTest.getBean(TestBean.class);
        Assert.assertNotNull(result);
    }

    @Test
    public void testSetCfgContext() throws NoSuchFieldException {
        final ConfigurableApplicationContext cfgContext = mock(ConfigurableApplicationContext.class);
        springBeanUtilsUnderTest.setApplicationContext(cfgContext);
        Assert.assertNotNull(springBeanUtilsUnderTest.getClass().getDeclaredField("applicationContext"));
    }

    @Test
    public void testGetInstance() {
        final SpringBeanUtils result = SpringBeanUtils.getInstance();
        Assert.assertNotNull(result);
    }

    /**
     * Test Bean.
     */
    static final class TestBean {
    }
}
