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

package org.apache.shenyu.examples.dubbo.api.service;

import org.apache.shenyu.examples.dubbo.api.entity.DubboTest;
import org.apache.shenyu.examples.dubbo.api.entity.ListResp;

/**
 * DubboClassTestService.
 */
public interface DubboClassTestService {

    /**
     * find by id.
     * body：{"id":"1223"}
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
     * Insert dubbo test.
     * body :{"id":"122344","name":"xiaoyu"}
     *
     * @param dubboTest the dubbo test
     * @return the dubbo test
     */
    DubboTest insert(DubboTest dubboTest);

    /**
     * findList.
     *
     * @return {@linkplain ListResp}
     */
    ListResp findList();
}
