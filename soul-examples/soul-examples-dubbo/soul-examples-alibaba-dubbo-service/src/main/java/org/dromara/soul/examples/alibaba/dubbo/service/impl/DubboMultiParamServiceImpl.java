/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 *
 */

package org.dromara.soul.examples.alibaba.dubbo.service.impl;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.dromara.soul.client.dubbo.common.annotation.SoulDubboClient;
import org.dromara.soul.examples.dubbo.api.entity.ComplexBeanTest;
import org.dromara.soul.examples.dubbo.api.entity.DubboTest;
import org.dromara.soul.examples.dubbo.api.service.DubboMultiParamService;
import org.springframework.stereotype.Service;

/**
 * The type Dubbo multi param service.
 */
@Service("dubboMultiParamService")
public class DubboMultiParamServiceImpl implements DubboMultiParamService {

    @Override
    @SoulDubboClient(path = "/findByIdsAndName", desc = "Query by Ids and name")
    public DubboTest findByIdsAndName(List<Integer> ids, String name) {
        DubboTest test = new DubboTest();
        test.setId(ids.toString());
        test.setName("hello world soul alibaba dubbo param findByIdsAndName ：" + name);
        return test;
    }

    @Override
    @SoulDubboClient(path = "/findByArrayIdsAndName")
    public DubboTest findByArrayIdsAndName(Integer[] ids, String name) {
        DubboTest test = new DubboTest();
        test.setId(Arrays.toString(ids));
        test.setName("hello world soul alibaba dubbo param findByArrayIdsAndName ：" + name);
        return test;
    }

    @Override
    @SoulDubboClient(path = "/findByStringArray")
    public DubboTest findByStringArray(String[] ids) {
        DubboTest test = new DubboTest();
        test.setId(Arrays.toString(ids));
        test.setName("hello world soul alibaba dubbo param findByStringArray");
        return test;
    }

    @Override
    @SoulDubboClient(path = "/findByListId")
    public DubboTest findByListId(List<String> ids) {
        DubboTest test = new DubboTest();
        test.setId(ids.toString());
        test.setName("hello world soul alibaba dubbo param findByListId");
        return test;
    }

    @Override
    @SoulDubboClient(path = "/batchSave")
    public DubboTest batchSave(List<DubboTest> dubboTestList) {
        DubboTest test = new DubboTest();
        test.setId(dubboTestList.stream().map(DubboTest::getId).collect(Collectors.joining("-")));
        test.setName("hello world soul alibaba dubbo param batchSave :" + dubboTestList.stream().map(DubboTest::getName).collect(Collectors.joining("-")));
        return test;
    }

    @Override
    @SoulDubboClient(path = "/batchSaveAndNameAndId")
    public DubboTest batchSaveAndNameAndId(List<DubboTest> dubboTestList, String id, String name) {
        DubboTest test = new DubboTest();
        test.setId(id);
        test.setName("hello world soul alibaba dubbo param batchSaveAndNameAndId :" + name + ":" + dubboTestList.stream().map(DubboTest::getName).collect(Collectors.joining("-")));
        return test;
    }

    @Override
    @SoulDubboClient(path = "/saveComplexBeanTest")
    public DubboTest saveComplexBeanTest(ComplexBeanTest complexBeanTest) {
        DubboTest test = new DubboTest();
        test.setId(complexBeanTest.getIdLists().toString());
        test.setName("hello world soul alibaba dubbo param saveComplexBeanTest :" + complexBeanTest.getDubboTest().getName());
        return test;
    }

    @Override
    @SoulDubboClient(path = "/saveComplexBeanTestAndName")
    public DubboTest saveComplexBeanTestAndName(ComplexBeanTest complexBeanTest, String name) {
        DubboTest test = new DubboTest();
        test.setId(complexBeanTest.getIdLists().toString());
        test.setName("hello world soul alibaba dubbo param saveComplexBeanTestAndName :" + complexBeanTest.getDubboTest().getName() + "-" + name);
        return test;
    }

}
