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

package org.apache.shenyu.examples.alibaba.dubbo.service.annotation.impl;

import com.alibaba.dubbo.config.annotation.Service;
import org.apache.shenyu.client.apidocs.annotations.ApiDoc;
import org.apache.shenyu.client.apidocs.annotations.ApiModule;
import org.apache.shenyu.client.dubbo.common.annotation.ShenyuDubboClient;
import org.apache.shenyu.examples.common.aop.Log;
import org.apache.shenyu.examples.dubbo.api.entity.DubboTest;
import org.apache.shenyu.examples.dubbo.api.entity.ListResp;
import org.apache.shenyu.examples.dubbo.api.service.DubboTestService;

import java.util.Collections;
import java.util.Random;

/**
 * The type Dubbo service.
 */
@Service
@ApiModule(value = "dubboTestService")
public class DubboTestServiceImpl implements DubboTestService {
    
    @Override
    @ShenyuDubboClient("/findById")
    @Log
    @ApiDoc(desc = "findById")
    public DubboTest findById(final String id) {
        return new DubboTest(id, "hello world shenyu Alibaba Dubbo, findById");
    }
    
    @Override
    @ShenyuDubboClient("/findAll")
    @ApiDoc(desc = "findAll")
    public DubboTest findAll() {
        return new DubboTest(String.valueOf(new Random().nextInt()), "hello world shenyu Alibaba Dubbo , findAll");
    }
    
    @Override
    @ShenyuDubboClient("/insert")
    @ApiDoc(desc = "insert")
    public DubboTest insert(final DubboTest dubboTest) {
        dubboTest.setName("hello world shenyu Alibaba Dubbo: " + dubboTest.getName());
        return dubboTest;
    }
    
    @Override
    @ShenyuDubboClient("/findList")
    @ApiDoc(desc = "findList")
    public ListResp findList() {
        return new ListResp(1, Collections.singletonList(new DubboTest("1", "test")));
    }
}
