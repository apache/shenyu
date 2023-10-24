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
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class ShenyuApacheDubboClientAplFallBack implements ShenyuApacheDubboClientApi {

    @Override
    public DubboTest findAll() {

        DubboTest dubboTest = new DubboTest();
        dubboTest.setId("a");
        dubboTest.setName("fallback");
        return dubboTest;
    }

    @Override
    public ListResp findList() {
        return null;
    }

    @Override
    public DubboTest findById(final String id) {
        return null;
    }

    @Override
    public DubboTest insert(final DubboTest dubboTest) {
        return null;
    }

    @Override
    public DubboTest findByListId(final List<String> ids) {
        DubboTest dubboTest = new DubboTest();
        dubboTest.setId("a");
        dubboTest.setName("fallback");
        return dubboTest;
    }

    @Override
    public DubboTest findByIdsAndName(final DubboRequestBody dubboRequestBody) {
        return null;
    }

    @Override
    public DubboTest findByArrayIdsAndName(final DubboRequestBody dubboRequestBody) {
        return null;
    }

    @Override
    public DubboTest saveComplexBeanTest(final ComplexBeanTest complexBeanTest) {
        return null;
    }

    @Override
    public DubboTest batchSave(final DubboTestSaveRequest dubboTestSaveRequest) {
        return null;
    }

    @Override
    public DubboTest batchSaveAndNameAndId(final DubboTestSaveRequest dubboTestSaveRequest) {
        return null;
    }
}
