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

package org.apache.shenyu.common.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

class NamespaceIDUtilsTest {

    @Test
    void testGetInstance() {

        NamespaceIDUtils instance1 = NamespaceIDUtils.getInstance();
        NamespaceIDUtils instance2 = NamespaceIDUtils.getInstance();
        assertNotNull(instance1, "Instance should not be null");
        assertSame(instance1, instance2, "Instances should be the same (singleton)");
    }

    @Test
    void testGenerateNamespaceID() {

        NamespaceIDUtils namespaceIDUtils = NamespaceIDUtils.getInstance();
        String namespaceID1 = namespaceIDUtils.generateNamespaceID();
        String namespaceID2 = namespaceIDUtils.generateNamespaceID();

        assertNotNull(namespaceID1, "Generated namespace ID should not be null");
        assertNotNull(namespaceID2, "Generated namespace ID should not be null");
        assertNotEquals(namespaceID1, namespaceID2, "Generated namespace IDs should be unique");
    }

}
