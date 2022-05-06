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

import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class FreshBeanHolderTest {

    @Test
    public void testApply() {
        Function<String, String> function = Function.identity();
        FreshBeanHolder<String, String> freshBeanHolder = new FreshBeanHolder<>(function);
        assertEquals("hello", freshBeanHolder.apply("hello"));
    }

    @Test
    public void testDoFresh() {
        Function<String, String> function = Function.identity();
        FreshBeanHolder<String, String> freshBeanHolder = new FreshBeanHolder<>(function);
        freshBeanHolder.doFresh("hello world");
        assertEquals("hello world", freshBeanHolder.apply("hello"));
    }

    @Test
    public void testInit() {
        Function<String, String> function = Function.identity();
        FreshBeanHolder<String, String> freshBeanHolder = new FreshBeanHolder<>(function);
        freshBeanHolder.init("hello world");
        assertEquals("hello world", freshBeanHolder.init("hello"));
    }
}
