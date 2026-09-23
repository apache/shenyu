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

package org.apache.shenyu.plugin.sign.service;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link ComposableSignService}.
 */
public final class ComposableSignServiceTest {

    @Test
    public void testMatchesDefaultModule() {
        assertTrue(ComposableSignService.matchesDefaultModule("divide-http", "http", "divide"));
        assertTrue(ComposableSignService.matchesDefaultModule("springCloud-springCloud", "springCloud", "springCloud"));
        assertTrue(ComposableSignService.matchesDefaultModule("divide-", "", "divide"));
        assertTrue(ComposableSignService.matchesDefaultModule("divide-null", null, "divide"));
        assertFalse(ComposableSignService.matchesDefaultModule("divide-http", "dubbo", "divide"));
        assertFalse(ComposableSignService.matchesDefaultModule("custom-http", "http", "divide"));
        assertFalse(ComposableSignService.matchesDefaultModule(null, "http", "divide"));
    }
}
