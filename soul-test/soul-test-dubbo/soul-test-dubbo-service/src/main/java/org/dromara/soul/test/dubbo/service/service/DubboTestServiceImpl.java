/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.test.dubbo.service.service;

import org.dromara.soul.test.dubbo.api.entity.DubboTest;
import org.dromara.soul.test.dubbo.api.service.DubboTestService;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * DubboTestServiceImpl.
 *
 * @author xiaoyu(Myth)
 */
@Service("dubboTestService")
public class DubboTestServiceImpl implements DubboTestService {

    @Override
    public DubboTest findById(String id) {
        DubboTest dubboTest = new DubboTest();
        dubboTest.setName("hhah");
        return dubboTest;
    }

    @Override
    public DubboTest insert(DubboTest dubboTest) {
        return dubboTest;
    }

    @Override
    public DubboTest insert3(DubboTest dubboTest, String id, String name) {
        DubboTest test = new DubboTest();
        test.setId("test3" + id);
        test.setName("xiaoyu3" + name);
        return test;
    }

    @Override
    public DubboTest findByIdAndName(String id, String name) {
        DubboTest test = new DubboTest();
        test.setName("测试调用多个String类型参数");
        return test;
    }

    @Override
    public DubboTest testEntityStringListParam(DubboTest dubboTest, String id, List<String> ids) {
        DubboTest test = new DubboTest();
        test.setName("测试调用参数为实体与String类型，List类型参数");
        return test;
    }

    @Override
    public DubboTest testEntityStringParam(DubboTest dubboTest, String id, Integer name) {
        DubboTest test = new DubboTest();
        test.setName("测试调用参数为实体与String类型参数");
        return test;
    }

    @Override
    public DubboTest testMultiEntity(DubboTest test1, DubboTest test2) {
        DubboTest test = new DubboTest();
        test.setName("测试调用多个实体参数");
        test.setId("2222");
        return test;
    }

    @Override
    public DubboTest testListEntity(List<DubboTest> dubboTestList) {
        DubboTest test = new DubboTest();
        test.setName("测试调用List泛型实体参数");
        test.setId("111111");
        return test;
    }
}
