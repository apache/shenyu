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
     * @return DubboTest dubbo test
     */
    DubboTest findById(String id);


    /**
     * Find all dubbo test.
     *
     * @return the dubbo test
     */
    DubboTest findAll();


    /**
     * Find by long dubbo test.
     *
     * @param id the id
     * @return the dubbo test
     */
    String findByLong(Long id);


    /**
     * Insert dubbo test.
     *
     * @param dubboTest the dubbo test
     * @return the dubbo test
     */
    DubboTest insert(DubboTest dubboTest);


    /**
     * Insert 3 dubbo test.
     *
     * @param dubboTest the dubbo test
     * @param id        the id
     * @param name      the name
     * @return the dubbo test
     */
    DubboTest insert3(DubboTest dubboTest, String id, String name);

    /**
     * findByIdAndName.
     *
     * @param id   id
     * @param name name
     * @return DubboTest dubbo test
     */
    DubboTest findByIdAndName(String id, String name);


    /**
     * Test entity string list param dubbo test.
     *
     * @param dubboTest the dubbo test
     * @param id        the id
     * @param ids       the ids
     * @return the dubbo test
     */
    DubboTest testEntityStringListParam(DubboTest dubboTest, String id, List<String> ids);

    /**
     * Test entity string param dubbo test.
     *
     * @param dubboTest the dubbo test
     * @param id        the id
     * @param name      the name
     * @return the dubbo test
     */
    DubboTest testEntityStringParam(DubboTest dubboTest, String id, Integer name);

    /**
     * Test multi entity dubbo test.
     *
     * @param test1 the test 1
     * @param test2 the test 2
     * @return the dubbo test
     */
    DubboTest testMultiEntity(DubboTest test1, DubboTest test2);

    /**
     * Test list entity dubbo test.
     *
     * @param dubboTestList the dubbo test list
     * @return the dubbo test
     */
    DubboTest testListEntity(List<DubboTest> dubboTestList);

}
