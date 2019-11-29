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

import org.dromara.soul.client.common.annotation.SoulClient;
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
    @SoulClient(path = "/findById", desc = "根据用户查询")
    public DubboTest findById(final String id) {
        DubboTest dubboTest = new DubboTest();
        dubboTest.setName("hhah");
        return dubboTest;
    }

    @Override
    @SoulClient(path = "/findAll", desc = "获取所有")
    public DubboTest findAll() {
        DubboTest dubboTest = new DubboTest();
        dubboTest.setName("findAll");
        return dubboTest;
    }

    @Override
    @SoulClient(path = "/findByLong", desc = "findByLong")
    public String findByLong(final Long id) {
        return "Long id" + id;
    }

    @Override
    @SoulClient(path = "/insert", desc = "插入一条数据")
    public DubboTest insert(final DubboTest dubboTest) {
        return dubboTest;
    }

}
