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

package org.apache.shenyu.examples.sdk.alibaba.dubbo.consumer.dto;

import java.util.List;
import org.apache.shenyu.examples.dubbo.api.entity.DubboTest;

/**
 * DubboTestSaveRequest.
 */
public class DubboTestSaveRequest {

    /**
     * dubboTestList.
     */
    private List<DubboTest> dubboTestList;

    /**
     * id.
     */
    private String id;

    /**
     * name.
     */
    private String name;

    /**
     * getId.
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * setId.
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * getName.
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * setName.
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * getDubboTestList.
     * @return list
     */
    public List<DubboTest> getDubboTestList() {
        return dubboTestList;
    }

    /**
     * setDubboTestList.
     * @param dubboTestList dubboTestList
     */
    public void setDubboTestList(final List<DubboTest> dubboTestList) {
        this.dubboTestList = dubboTestList;
    }
}
