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

package org.dromara.soul.boostrap;

import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.web.dubbo.DubboMultiParameterResolveServiceImpl;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

public final class DubboMultiParameterTest {

    private final DubboMultiParameterResolveServiceImpl resolveService = new DubboMultiParameterResolveServiceImpl();
    
    @Test
    public void assertTwoStringParam() {
        final String body = "{\"id\": \"123\",\"name\": \"xiaoyu\"}";
        final String parameterTypes = "java.lang.String,java.lang.String";
        Pair<String[], Object[]> pair = resolveService.buildParameter(body, parameterTypes);
        assertThat(pair.getLeft().length, is(2));
        assertThat(pair.getRight().length, is(2));
    }
    
    @Test
    public void assertArrayAndString() {
        final String body = "{\"ids\":[\"123\",\"456\"],\"name\":\"hello world\"}\n";
        final String parameterTypes = "java.lang.Integer[],java.lang.String";
        Pair<String[], Object[]> pair = resolveService.buildParameter(body, parameterTypes);
        assertThat(pair.getLeft().length, is(2));
        assertThat(pair.getRight().length, is(2));
    }
}

