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

/**
 * Request object contains id array and name property.
 */
public class IdArrayAndNameRequest {
    /**
     * Request param id array.
     */
    private Integer[] ids;

    /**
     * Request param name.
     */
    private String name;

    public IdArrayAndNameRequest() { }

    public IdArrayAndNameRequest(final Integer[] ids, final String name) {
        this.ids = ids;
        this.name = name;
    }

    /**
     * Get request param name.
     * @return name Request param name.
     */
    public String getName() {
        return name;
    }

    /**
     * Set request param name.
     * @param name Request param name.
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * Return request param id list.
     * @return ids Request param id list.
     */
    public Integer[] getIds() {
        return ids;
    }

    /**
     * Set request param id list.
     * @param ids Request param id list.
     */
    public void setIds(final Integer[] ids) {
        this.ids = ids;
    }
}
