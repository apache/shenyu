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


package org.apache.shenyu.client.core.register;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;

/**
 * Tests ownership and mutation isolation of copied API definitions.
 */
public class ApiBeanCopyTest {

    @Test
    public void testCopiedDefinitionsUseCopiedBeanPath() throws NoSuchMethodException {
        ApiBean original = new ApiBean("http", "bean", new Object(), "/original");
        original.addProperties("bean-property", "original");
        original.addApiDefinition(Object.class.getMethod("toString"), "/first");
        original.addApiDefinition(Object.class.getMethod("hashCode"), "/second");
        original.getApiDefinitions().get(0).addProperties("api-property", "original");
        ApiBean copy = original.copy();
        copy.setBeanPath("/processed");
        copy.addProperties("bean-property", "copied");
        copy.getApiDefinitions().get(0).addProperties("api-property", "copied");
        copy.getApiDefinitions().get(0).setMethodPath("/changed");
        assertSame(original.getBeanInstance(), copy.getBeanInstance());
        for (int i = 0; i < original.getApiDefinitions().size(); i++) {
            ApiBean.ApiDefinition copiedDefinition = copy.getApiDefinitions().get(i);
            ApiBean.ApiDefinition originalDefinition = original.getApiDefinitions().get(i);
            assertNotSame(originalDefinition, copiedDefinition);
            assertSame(copy, copiedDefinition.getApiBean());
            assertSame(original, originalDefinition.getApiBean());
            assertEquals("/processed", copiedDefinition.getBeanPath());
            assertEquals("/original", originalDefinition.getBeanPath());
            assertEquals(originalDefinition.getApiMethod(), copiedDefinition.getApiMethod());
        }
        assertEquals("original", original.getPropertiesValue("bean-property"));
        assertEquals("original", original.getApiDefinitions().get(0).getPropertiesValue("api-property"));
        assertEquals("/first", original.getApiDefinitions().get(0).getMethodPath());
        assertEquals("/second", copy.getApiDefinitions().get(1).getMethodPath());
    }
}

