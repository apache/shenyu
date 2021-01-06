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

package org.dromara.soul.client.springmvc.utils;

import org.dromara.soul.client.springmvc.config.SoulSpringMvcConfig;
import org.junit.Test;

/**
 * Test cases for ValidateUtils.
 *
 * @author severez
 */
public class ValidateUtilsTest {

    @Test
    public void testValidatePass() {
        SoulSpringMvcConfig soulSpringMvcConfig = new SoulSpringMvcConfig();
        soulSpringMvcConfig.setAdminUrl("http://127.0.0.1:8080");
        soulSpringMvcConfig.setContextPath("/test");
        soulSpringMvcConfig.setPort(8081);
        ValidateUtils.validate(soulSpringMvcConfig);
    }

    @Test(expected = RuntimeException.class)
    public void testValidateThrowException() {
        ValidateUtils.validate(new SoulSpringMvcConfig());
    }
}
