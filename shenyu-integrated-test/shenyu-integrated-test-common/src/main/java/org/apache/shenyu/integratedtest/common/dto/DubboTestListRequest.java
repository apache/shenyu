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

package org.apache.shenyu.integratedtest.common.dto;

import java.util.List;

/**
 * Request object contains DubboTest list property.
 */
public class DubboTestListRequest {
    /**
     * Request param DubboTest list.
     */
    private List<DubboTest> dubboTestList;

    public DubboTestListRequest() { }

    public DubboTestListRequest(final List<DubboTest> dubboTestList) {
        this.dubboTestList = dubboTestList;
    }

    /**
     * Return request param DubboTest list.
     * @return dubboTestList Request param DubboTest list.
     */
    public List<DubboTest> getDubboTestList() {
        return dubboTestList;
    }

    /**
     * Set request param DubboTest list.
     * @param dubboTestList Request param DubboTest list.
     */
    public void setDubboTestList(final List<DubboTest> dubboTestList) {
        this.dubboTestList = dubboTestList;
    }
}
