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

package org.apache.shenyu.examples.sdk.apache.dubbo.consumer.impl;

import org.apache.shenyu.examples.dubbo.api.entity.ComplexBeanTest;
import org.apache.shenyu.examples.dubbo.api.entity.DubboTest;
import org.apache.shenyu.examples.dubbo.api.entity.ListResp;
import org.apache.shenyu.examples.sdk.apache.dubbo.consumer.api.ShenyuApacheDubboClientApi;
import org.apache.shenyu.examples.sdk.apache.dubbo.consumer.dto.DubboRequestBody;
import org.apache.shenyu.examples.sdk.apache.dubbo.consumer.dto.DubboTestSaveRequest;
import org.apache.shenyu.sdk.spring.FallbackFactory;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class ShenyuApacheDubboClientAplFallBackFactory implements FallbackFactory<ShenyuApacheDubboClientApi> {

    @Override
    public ShenyuApacheDubboClientApi create(Throwable cause) {

        System.out.println("exception message:" + cause.getMessage());
        return new ShenyuApacheDubboClientApi() {
            @Override
            public DubboTest findAll() {

                return new DubboTest("1", "fallBackFactory");
            }

            @Override
            public ListResp findList() {

                List<DubboTest> list = new ArrayList<>();
                list.add(new DubboTest("1", "fallBackFactory"));
                list.add(new DubboTest("2", "fallBackFactory"));
                ListResp listResp = new ListResp(2, list);
                return listResp;
            }

            @Override
            public DubboTest findById(String id) {
                return null;
            }

            @Override
            public DubboTest insert(DubboTest dubboTest) {
                return null;
            }

            @Override
            public DubboTest findByListId(List<String> ids) {
                return null;
            }

            @Override
            public DubboTest findByIdsAndName(DubboRequestBody dubboRequestBody) {
                return null;
            }

            @Override
            public DubboTest findByArrayIdsAndName(DubboRequestBody dubboRequestBody) {
                return null;
            }

            @Override
            public DubboTest saveComplexBeanTest(ComplexBeanTest complexBeanTest) {
                return null;
            }

            @Override
            public DubboTest batchSave(DubboTestSaveRequest dubboTestSaveRequest) {
                return null;
            }

            @Override
            public DubboTest batchSaveAndNameAndId(DubboTestSaveRequest dubboTestSaveRequest) {
                return null;
            }
        };
    }
}
