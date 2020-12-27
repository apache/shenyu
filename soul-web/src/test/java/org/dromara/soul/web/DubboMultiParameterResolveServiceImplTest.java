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

package org.dromara.soul.web;

import lombok.Data;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.web.dubbo.DubboMultiParameterResolveServiceImpl;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Test cases for WebSocketParamFilter.
 *
 * @author candyYu
 */
@RunWith(MockitoJUnitRunner.class)
public final class DubboMultiParameterResolveServiceImplTest  {

    private final DubboMultiParameterResolveServiceImpl impl = new DubboMultiParameterResolveServiceImpl();

    @Test
    public void testBuildParameter() {
        String body = "{\"student\": {\"id\": 12345,\"name\": \"candyYu\"}}";
        String parameterTypes = "org.dromara.soul.web.DubboMultiParameterResolveServiceImplTest.Student";
        Pair<String[], Object[]> pair = impl.buildParameter(body, parameterTypes);
        assertThat(pair.getLeft().length, is(1));
        assertThat(pair.getRight().length, is(1));

        body = "{\"students\": [{\"id\": 12345,\"name\": \"candyYu\"},{\"id\": 12346,\"name\": \"candyYu2\"}]}";
        parameterTypes = "org.dromara.soul.web.DubboMultiParameterResolveServiceImplTest.Student[]";
        pair = impl.buildParameter(body, parameterTypes);
        assertThat(pair.getLeft().length, is(1));
        assertThat(pair.getRight().length, is(1));

        body = "{\"ids\":[\"123\",\"456\"],\"name\":\"hello world\"}\n";
        parameterTypes = "java.lang.Integer[],java.lang.String";
        pair = impl.buildParameter(body, parameterTypes);
        assertThat(pair.getLeft().length, is(2));
        assertThat(pair.getRight().length, is(2));

        body = "{\"ids\":[\"123\",\"456\"],\"name\":\"hello world\"}\n";
        parameterTypes = "java.lang.Integer[],java.lang.String";
        pair = impl.buildParameter(body, parameterTypes);
        assertThat(pair.getLeft().length, is(2));
        assertThat(pair.getRight().length, is(2));
    }

    @Data
    private final class Student {
        private long id;

        private String name;
    }
}
