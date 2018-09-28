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

package org.dromara.soul.test.dubbo.api.service;

import org.dromara.soul.test.dubbo.api.entity.DubboTest;

import java.util.List;

/**
 * DubboTestService.
 *
 * @author xiaoyu(Myth)
 */
public interface DubboTestService {

    /**
     * find by id.
     *
     * @param id id
     * @return DubboTest
     */
    DubboTest findById(String id);

    /**
     * insert .
     *
     * @param dubboTest dubboTest
     */
    DubboTest insert(DubboTest dubboTest);


    /**
     * insert by more param.
     *
     * @param dubboTest dubboTest
     */
    DubboTest insert3(DubboTest dubboTest, String id, String name);

    /**
     * findByIdAndName.
     *
     * @param id   id
     * @param name name
     * @return DubboTest
     */
    DubboTest findByIdAndName(String id, String name);


    DubboTest testEntityStringListParam(DubboTest dubboTest, String id, List<String> ids);

    DubboTest testEntityStringParam(DubboTest dubboTest, String id, Integer name);


    DubboTest testMultiEntity(DubboTest test1, DubboTest test2);

    DubboTest testListEntity(List<DubboTest> dubboTestList);

}
