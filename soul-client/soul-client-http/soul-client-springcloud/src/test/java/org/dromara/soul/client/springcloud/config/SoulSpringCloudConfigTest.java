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

package org.dromara.soul.client.springcloud.config;

import org.hamcrest.Matchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Test Case for SoulSpringCloudConfig.
 *
 * @author DaveModl (davemo-coderpersonal@hotmail.com)
 */
public final class SoulSpringCloudConfigTest {

    private static final String ADMIN_URL = "http;//127.0.0.1:8000";

    private static final String CONTEXT_PATH = "/test";

    private static final Boolean FULL = true;

    private SoulSpringCloudConfig testCase;

    @Before
    public void setUp() {
        testCase = new SoulSpringCloudConfig();
        testCase.setAdminUrl(ADMIN_URL);
        testCase.setContextPath(CONTEXT_PATH);
        testCase.setFull(FULL);
    }

    @Test
    public void testSoulSpringCloudConfigCreate() {
        Assert.assertNotNull(new SoulSpringCloudConfig());
        Assert.assertThat(testCase.getAdminUrl(), Matchers.equalTo(ADMIN_URL));
        Assert.assertThat(testCase.getContextPath(), Matchers.equalTo(CONTEXT_PATH));
        Assert.assertThat(testCase.isFull(), Matchers.equalTo(FULL));
    }
}
