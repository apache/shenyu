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

package org.dromara.soul.plugin.tars.cache;

import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.tars.proxy.TarsInvokePrxList;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * Test case for {@link ApplicationConfigCache}.
 *
 * @author HoldDie
 */
@RunWith(MockitoJUnitRunner.class)
public final class ApplicationConfigCacheTest {

    private ApplicationConfigCache applicationConfigCacheUnderTest;

    @Before
    public void setUp() {
        applicationConfigCacheUnderTest = ApplicationConfigCache.getInstance();
    }

    @Test
    public void testGet() {
        final MetaData metaData = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path", RpcTypeEnum.TARS.getName(), "serviceName", "method1",
                "parameterTypes", "{\"methodInfo\":[{\"methodName\":\"method1\",\"params\":[{\"key\":\"int\",\"value\":\"param1\"},{\"key\":\"java.lang.Integer\",\"value\":\"param2\"}]}]}", false);
        applicationConfigCacheUnderTest.initPrx(metaData);
        final TarsInvokePrxList result = applicationConfigCacheUnderTest.get("path");
        assertNotNull(result);
    }

    @Test
    public void testInitPrx() {
        final MetaData metaData = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path", RpcTypeEnum.TARS.getName(), "serviceName", "method1",
                "parameterTypes", "{\"methodInfo\":[{\"methodName\":\"method1\",\"params\":[{\"key\":\"int\",\"value\":\"param1\"},{\"key\":\"java.lang.Integer\",\"value\":\"param2\"}]}]}", false);
        applicationConfigCacheUnderTest.initPrx(metaData);
        assertNotNull(applicationConfigCacheUnderTest.get(metaData.getPath()));
    }

    @Test
    public void testGetClassMethodKey() {
        assertEquals("className_methodName", ApplicationConfigCache.getClassMethodKey("className", "methodName"));
    }

    @Test
    public void testGetInstance() {
        final ApplicationConfigCache result = ApplicationConfigCache.getInstance();
        assertNotNull(result);
    }
}
