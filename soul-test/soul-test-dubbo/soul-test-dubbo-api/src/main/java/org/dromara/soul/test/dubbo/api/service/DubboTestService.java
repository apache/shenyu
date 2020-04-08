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

import java.util.List;
import java.util.Map;

import org.dromara.soul.test.dubbo.api.entity.ComplexBeanTest;
import org.dromara.soul.test.dubbo.api.entity.DubboTest;

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
     * find by Double dubbo test
     *
     * @param id
     * @return
     */

    String findByDouble(Double id);

    /**
     * query dubbo by array
     *
     * @param ids
     * @return
     */
    String queryByArray(Integer[] ids);

    /**
     * query by String Array dubbo test
     *
     * @param ids
     * @return
     */
    String queryByStringArray(String[] ids);

    /**
     * query by list dubbo test
     *
     * @param ids
     * @return
     */
    String queryByList(List<Integer> ids);

    /**
     * query by complex list dubbo test
     *
     * @param complexBeanTests
     * @return
     */
    String queryByComplexList(List<ComplexBeanTest> complexBeanTests);

    /**
     * query by map dubbo test
     *
     * @param ids
     * @return
     */
    String queryByMap(Map<String, String> ids);

    /**
     * query by complex map dubbo test
     *
     * @param complexBeanTestMap
     * @return
     */
    String queryByComplexMap(Map<String, ComplexBeanTest> complexBeanTestMap);

    /**
     * insert complex data dubbo test
     *
     * @return
     */
    String insertComplexData(ComplexBeanTest complexBeanTest);


}
