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

package org.apache.shenyu.examples.http.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.io.Serializable;
import java.util.Date;
import java.util.StringJoiner;

/**
 * The type Order dto.
 */
public class OrderDTO implements Serializable {

    @Schema(name = "id", requiredMode = Schema.RequiredMode.REQUIRED, example = "100000")
    private String id;

    @Schema(name = "name", requiredMode = Schema.RequiredMode.REQUIRED, example = "jack")
    private String name;

    @Schema(name = "createTime", example = "2023-08-01 10:10:01")
    private Date createTime;

    /**
     * Get id.
     *
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * Set id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
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

    /**
     * get createTime.
     * @return createTime
     */
    public Date getCreateTime() {
        return createTime;
    }

    /**
     * setCreateTime.
     * @param createTime createTime
     */
    public void setCreateTime(final Date createTime) {
        this.createTime = createTime;
    }

    @Override
    public String toString() {
        return new StringJoiner(", ", OrderDTO.class.getSimpleName() + "[", "]")
            .add("id='" + id + "'")
            .add("name='" + name + "'")
            .add("createTime='" + createTime + "'")
            .toString();
    }

}
