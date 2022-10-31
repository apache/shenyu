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

import java.io.Serializable;

/**
 * The type Complex bean test with name request.
 */
public class ComplexBeanTestWithNameRequest implements Serializable {

    /**
     * Request param complexBeanTest.
     */
    private ComplexBeanTest complexBeanTest;

    /**
     * Request param name.
     */
    private String name;

    public ComplexBeanTestWithNameRequest() { }

    public ComplexBeanTestWithNameRequest(final ComplexBeanTest complexBeanTest, final String name) {
        this.complexBeanTest = complexBeanTest;
        this.name = name;
    }

    /**
     * Get complexBeanTest.
     *
     * @return complexBeanTest
     */
    public ComplexBeanTest getComplexBeanTest() {
        return complexBeanTest;
    }

    /**
     * Set complexBeanTest.
     *
     * @param complexBeanTest complexBeanTest
     */
    public void setComplexBeanTest(final ComplexBeanTest complexBeanTest) {
        this.complexBeanTest = complexBeanTest;
    }

    /**
     * Get name.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * Set name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }
}
