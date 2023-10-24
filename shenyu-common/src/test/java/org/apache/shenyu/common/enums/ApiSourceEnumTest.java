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

package org.apache.shenyu.common.enums;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test Cases for ApiSourceEnum.
 */
public class ApiSourceEnumTest {

    @Test
    public void testGetName() {
        assertEquals("swagger", ApiSourceEnum.SWAGGER.getName());
        assertEquals("annotation_generation", ApiSourceEnum.ANNOTATION_GENERATION.getName());
        assertEquals("create_manually", ApiSourceEnum.CREATE_MANUALLY.getName());
        assertEquals("import_swagger", ApiSourceEnum.IMPORT_SWAGGER.getName());
        assertEquals("import_yapi", ApiSourceEnum.IMPORT_YAPI.getName());
    }

    @Test
    public void testGetValue() {
        assertEquals(0, ApiSourceEnum.SWAGGER.getValue());
        assertEquals(1, ApiSourceEnum.ANNOTATION_GENERATION.getValue());
        assertEquals(2, ApiSourceEnum.CREATE_MANUALLY.getValue());
        assertEquals(3, ApiSourceEnum.IMPORT_SWAGGER.getValue());
        assertEquals(4, ApiSourceEnum.IMPORT_YAPI.getValue());
    }
}
