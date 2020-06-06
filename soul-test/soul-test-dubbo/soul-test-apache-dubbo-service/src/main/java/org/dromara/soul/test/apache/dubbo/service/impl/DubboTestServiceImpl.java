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

package org.dromara.soul.test.apache.dubbo.service.impl;

import java.util.Random;
import org.dromara.soul.client.dubbo.common.annotation.SoulDubboClient;
import org.dromara.soul.test.dubbo.api.entity.DubboTest;
import org.dromara.soul.test.dubbo.api.service.DubboTestService;
import org.springframework.stereotype.Service;

/**
 * DubboTestServiceImpl.
 *
 * @author xiaoyu(Myth)
 */
@Service("dubboTestService")
public class DubboTestServiceImpl implements DubboTestService {
    
    @Override
    @SoulDubboClient(path = "/findById", desc = "根据用户查询")
    public DubboTest findById(final String id) {
        DubboTest dubboTest = new DubboTest();
        dubboTest.setId(id);
        dubboTest.setName("hello world Soul Apache, findById");
        return dubboTest;
    }
    
    @Override
    @SoulDubboClient(path = "/findAll", desc = "获取所有")
    public DubboTest findAll() {
        DubboTest dubboTest = new DubboTest();
        dubboTest.setName("hello world Soul Apache, findAll");
        dubboTest.setId(String.valueOf(new Random().nextInt()));
        return dubboTest;
    }
    
    @Override
    @SoulDubboClient(path = "/insert", desc = "插入一条数据")
    public DubboTest insert(final DubboTest dubboTest) {
        dubboTest.setName("hello world Soul Apache Dubbo: " + dubboTest.getName());
        return dubboTest;
    }
}
