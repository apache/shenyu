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

package org.apache.shenyu.examples.sdk.apache.dubbo.consumer.controller;

import java.util.List;
import org.apache.shenyu.examples.dubbo.api.entity.ComplexBeanTest;
import org.apache.shenyu.examples.dubbo.api.entity.DubboTest;
import org.apache.shenyu.examples.dubbo.api.entity.ListResp;
import org.apache.shenyu.examples.sdk.apache.dubbo.consumer.api.ShenyuApacheDubboClientApi;
import org.apache.shenyu.examples.sdk.apache.dubbo.consumer.dto.DubboRequestBody;
import org.apache.shenyu.examples.sdk.apache.dubbo.consumer.dto.DubboTestSaveRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * ShenyuHttpSdkExampleController.
 * invoke shenyuSdkAPi
 */
@RestController
public class ShenyuApacheDubboSdkExampleController {

    @Autowired
    private ShenyuApacheDubboClientApi shenyuApacheDubboClientApi;

    /**
     * findAll.
     * @return DubboTest
     */
    @GetMapping("/sdk/dubbo/findAll")
    public DubboTest findAll() {
        return shenyuApacheDubboClientApi.findAll();
    }

    /**
     * findList.
     * @return ListResp
     */
    @GetMapping("/sdk/dubbo/findList")
    public ListResp findList() {
        return shenyuApacheDubboClientApi.findList();
    }

    /**
     * findById.
     * @param id id
     * @return DubboTest
     */
    @GetMapping("/sdk/dubbo/findById")
    public DubboTest findById(final @RequestParam("id") String id) {
        return shenyuApacheDubboClientApi.findById(id);
    }

    /**
     * findByListId.
     * @param ids ids
     * @return DubboTest
     */
    @PostMapping("/sdk/dubbo/findByListId")
    public DubboTest findByListId(final @RequestBody List<String> ids) {
        return shenyuApacheDubboClientApi.findByListId(ids);
    }

    /**
     * insert.
     * @param dubboTest dubboTest
     * @return DubboTest
     */
    @PostMapping("/sdk/dubbo/insert")
    public DubboTest insert(final @RequestBody DubboTest dubboTest) {
        return shenyuApacheDubboClientApi.insert(dubboTest);
    }

    /**
     * findByIdsAndName.
     * @param dubboRequestBody dubboRequestBody
     * @return DubboTest
     */
    @PostMapping("/sdk/dubbo/demo/findByIdsAndName")
    public DubboTest findByIdsAndName(final @RequestBody DubboRequestBody dubboRequestBody) {
        return shenyuApacheDubboClientApi.findByIdsAndName(dubboRequestBody);
    }

    /**
     * findByArrayIdsAndName.
     * @param dubboRequestBody dubboRequestBody
     * @return DubboTest
     */
    @PostMapping("/sdk/dubbo/findByArrayIdsAndName")
    public DubboTest findByArrayIdsAndName(final @RequestBody DubboRequestBody dubboRequestBody) {
        return shenyuApacheDubboClientApi.findByArrayIdsAndName(dubboRequestBody);
    }

    /**
     * saveComplexBeanTest.
     * @param complexBeanTest complexBeanTest
     * @return DubboTest
     */
    @PostMapping("/sdk/dubbo/saveComplexBeanTest")
    public DubboTest saveComplexBeanTest(final @RequestBody ComplexBeanTest complexBeanTest) {
        return shenyuApacheDubboClientApi.saveComplexBeanTest(complexBeanTest);
    }

    /**
     * batchSave.
     * @param dubboTestSaveRequest dubboTestSaveRequest
     * @return DubboTest
     */
    @PostMapping("/sdk/dubbo/batchSave")
    public DubboTest batchSave(final @RequestBody DubboTestSaveRequest dubboTestSaveRequest) {
        return shenyuApacheDubboClientApi.batchSave(dubboTestSaveRequest);
    }

    /**
     * batchSaveAndNameAndId.
     * @param dubboTestSaveRequest dubboTestSaveRequest
     * @return DubboTest
     */
    @PostMapping("/sdk/dubbo/batchSaveAndNameAndId")
    public DubboTest batchSaveAndNameAndId(final @RequestBody DubboTestSaveRequest dubboTestSaveRequest) {
        return shenyuApacheDubboClientApi.batchSaveAndNameAndId(dubboTestSaveRequest);
    }

}
