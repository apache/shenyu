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

package org.apache.shenyu.examples.sdk.apache.dubbo.consumer.api;

import org.apache.shenyu.examples.dubbo.api.entity.ComplexBeanTest;
import org.apache.shenyu.examples.dubbo.api.entity.DubboTest;
import org.apache.shenyu.examples.dubbo.api.entity.ListResp;
import org.apache.shenyu.examples.sdk.apache.dubbo.consumer.dto.DubboRequestBody;
import org.apache.shenyu.examples.sdk.apache.dubbo.consumer.dto.DubboTestSaveRequest;
import org.apache.shenyu.examples.sdk.apache.dubbo.consumer.impl.ShenyuApacheDubboClientAplFallBack;
import org.apache.shenyu.sdk.spring.ShenyuClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

/**
 * ShenyuApacheDubboClientApi.
 */
@ShenyuClient(value = "shenyu-gateway", path = "/dubbo/demo", fallback = ShenyuApacheDubboClientAplFallBack.class)
public interface ShenyuApacheDubboClientApi {

    /**
     * findAll.
     * test Get.
     *
     * @return DubboTest
     */
    @GetMapping("findAll")
    DubboTest findAll();

    /**
     * findList.
     *
     * @return ListResp
     */
    @GetMapping("findList")
    ListResp findList();

    /**
     * findById.
     *
     * @param id id
     * @return DubboTest
     */
    @GetMapping("findById")
    DubboTest findById(@RequestParam("id") String id);

    /**
     * insert.
     *
     * @param dubboTest dubboTest
     * @return DubboTest
     */
    @PostMapping("insert")
    DubboTest insert(@RequestBody DubboTest dubboTest);

    /**
     * findByListId.
     *
     * @param ids ids
     * @return DubboTest
     */
    @PostMapping("findByListId")
    DubboTest findByListId(@RequestBody List<String> ids);

    /**
     * findByIdsAndName.
     *
     * @param dubboRequestBody dubboRequestBody
     * @return DubboTest
     */
    @PostMapping("findByIdsAndName")
    DubboTest findByIdsAndName(@RequestBody DubboRequestBody dubboRequestBody);

    /**
     * findByArrayIdsAndName.
     *
     * @param dubboRequestBody dubboRequestBody
     * @return DubboTest
     */
    @PostMapping("findByIdsAndName")
    DubboTest findByArrayIdsAndName(@RequestBody DubboRequestBody dubboRequestBody);

    /**
     * saveComplexBeanTest.
     *
     * @param complexBeanTest complexBeanTest
     * @return DubboTest
     */
    @PostMapping("saveComplexBeanTest")
    DubboTest saveComplexBeanTest(@RequestBody ComplexBeanTest complexBeanTest);

    /**
     * batchSave.
     *
     * @param dubboTestSaveRequest dubboTestSaveRequest
     * @return DubboTest
     */
    @PostMapping("batchSave")
    DubboTest batchSave(@RequestBody DubboTestSaveRequest dubboTestSaveRequest);

    /**
     * batchSaveAndNameAndId.
     *
     * @param dubboTestSaveRequest dubboTestSaveRequest
     * @return DubboTest
     */
    @PostMapping("batchSaveAndNameAndId")
    DubboTest batchSaveAndNameAndId(@RequestBody DubboTestSaveRequest dubboTestSaveRequest);
}
