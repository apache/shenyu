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

package org.apache.shenyu.examples.apache.dubbo.service.annotation.impl;

import org.apache.shenyu.client.apache.dubbo.annotation.ShenyuDubboService;
import org.apache.shenyu.client.dubbo.common.annotation.ShenyuDubboClient;
import org.apache.shenyu.examples.dubbo.api.entity.ComplexBeanTest;
import org.apache.shenyu.examples.dubbo.api.entity.DubboTest;
import org.apache.shenyu.examples.dubbo.api.service.DubboClassMultiParamService;
import org.springframework.lang.NonNull;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * The type Dubbo multi param service.
 */
@ShenyuDubboService("/demo")
public class DubboClassMultiParamServiceImpl implements DubboClassMultiParamService {
    
    @Override
    @ShenyuDubboClient("/findByIdsAndName")
    public DubboTest findByIdsAndName(final List<Integer> ids, final String name) {
        return new DubboTest(ids.toString(), "hello world shenyu apache dubbo param findByIdsAndName ：" + name);
    }

    @Override
    @ShenyuDubboClient("/findByArrayIdsAndName")
    public DubboTest findByArrayIdsAndName(final Integer[] ids, final String name) {
        return new DubboTest(Arrays.toString(ids), "hello world shenyu apache dubbo param findByArrayIdsAndName ：" + name);
    }

    @Override
    @ShenyuDubboClient("/findByStringArray")
    public DubboTest findByStringArray(final String[] ids) {
        return new DubboTest(Arrays.toString(ids), "hello world shenyu apache dubbo param findByStringArray");
    }

    @Override
    @ShenyuDubboClient("/findByListId")
    public DubboTest findByListId(final List<String> ids) {
        return new DubboTest(ids.toString(), "hello world shenyu apache dubbo param findByListId");
    }

    @Override
    @ShenyuDubboClient("/batchSave")
    public DubboTest batchSave(final List<DubboTest> dubboTestList) {
        return new DubboTest(join(dubboTestList, DubboTest::getId),
                "hello world shenyu apache dubbo param batchSave :" + join(dubboTestList, DubboTest::getName));
    }

    @Override
    @ShenyuDubboClient("/batchSaveAndNameAndId")
    public DubboTest batchSaveAndNameAndId(final List<DubboTest> dubboTestList, final String id, final String name) {
        return new DubboTest(id, "hello world shenyu apache dubbo param batchSaveAndNameAndId :"
                + name + ":" + join(dubboTestList, DubboTest::getName));
    }

    @Override
    @ShenyuDubboClient("/saveComplexBeanTest")
    public DubboTest saveComplexBeanTest(final ComplexBeanTest complexBeanTest) {
        return new DubboTest(complexBeanTest.getIdLists().toString(),
                "hello world shenyu apache dubbo param saveComplexBeanTest :" + complexBeanTest.getDubboTest().getName());
    }

    @Override
    @ShenyuDubboClient("/saveComplexBeanTestAndName")
    public DubboTest saveComplexBeanTestAndName(final ComplexBeanTest complexBeanTest, final String name) {
        return new DubboTest(complexBeanTest.getIdLists().toString(),
                "hello world shenyu alibaba dubbo param saveComplexBeanTestAndName :" + complexBeanTest.getDubboTest().getName() + "-" + name);
    }

    @Override
    @ShenyuDubboClient("/bigRequestBody")
    public DubboTest saveBigRequestBody(final DubboTest dubboTest) {
        return dubboTest;
    }
    
    private <T> String join(final @NonNull List<T> list, final Function<T, String> mapper) {
        return list.stream()
                .map(mapper)
                .collect(Collectors.joining("-"));
    }
}
