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

package org.apache.shenyu.admin.config;

import io.swagger.v3.oas.models.OpenAPI;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for SwaggerConfiguration.
 */
@ExtendWith(MockitoExtension.class)
public final class SwaggerConfigurationTest {

    @InjectMocks
    private SwaggerConfiguration swaggerConfiguration;

    @Test
    public void testApiInfo() {
        OpenAPI actual = swaggerConfiguration.apiInfo();
        assertNotNull(actual);
        Assertions.assertEquals(1, actual.getSecurity().size());
        Assertions.assertEquals(1, actual.getSecurity().get(0).size());
        Assertions.assertTrue(actual.getSecurity().get(0).containsKey(org.apache.shenyu.common.constant.Constants.X_ACCESS_TOKEN));
    }
}
