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

package org.apache.shenyu.plugin.dubbo.common.param;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Test cases for DefaultDubboParamResolveService.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DubboParamResolveServiceTest {
    
    @InjectMocks
    private DubboParamResolveServiceImpl impl;
    
    @Test
    public void testBuildParameterWithNull() {
        String body = "{\"id\":null,\"name\":null}";
        String parameterTypes = "org.apache.shenyu.web.rpc.DubboMultiParameterResolveServiceImplTest.Student";
        Pair<String[], Object[]> pair = impl.buildParameter(body, parameterTypes);
        assertThat(pair.getLeft().length, is(1));
        assertThat(pair.getRight().length, is(1));
        Map<?, ?> map = (HashMap<?, ?>) pair.getRight()[0];
        assertNull(map.get("id"));
        assertNull(map.get("name"));
        
        body = "{\"dubboTest\":{\"id\":null,\"name\":null},\"idLists\":[null,null],\"idMaps\":{\"id2\":null,\"id1\":null}}";
        parameterTypes = "org.apache.shenyu.web.rpc.DubboMultiParameterResolveServiceImplTest.ComplexBean";
        pair = impl.buildParameter(body, parameterTypes);
        assertThat(pair.getLeft().length, is(1));
        assertThat(pair.getRight().length, is(1));
        map = (Map<?, ?>) pair.getRight()[0];
        Map<?, ?> dubboTest = (Map<?, ?>) map.get("dubboTest");
        assertNull(dubboTest.get("id"));
        assertNull(dubboTest.get("name"));
        List<?> idList = (List<?>) map.get("idLists");
        assertNull(idList.get(0));
        assertNull(idList.get(1));

        body = "{\"complexBean\":{\"dubboTest\":{\"id\":null,\"name\":null},\"idLists\":[null,null],\"idMaps\":{\"id2\":null,\"id1\":null}},\"name\":null}";
        parameterTypes = "{\"complexBean\":\"org.apache.shenyu.web.rpc.DubboMultiParameterResolveServiceImplTest.ComplexBean\",\"name\":\"java.lang.String\"}";
        pair = impl.buildParameter(body, parameterTypes);
        assertThat(pair.getLeft().length, is(2));
        assertThat(pair.getRight().length, is(2));
        map = (Map<?, ?>) pair.getRight()[0];
        Map<?, ?> dubboTest1 = (Map<?, ?>) map.get("dubboTest");
        assertNull(dubboTest1.get("id"));
        assertNull(dubboTest1.get("name"));
        List<?> idList1 = (List<?>) map.get("idLists");
        assertNull(idList1.get(0));
        assertNull(idList1.get(1));
    }
}

