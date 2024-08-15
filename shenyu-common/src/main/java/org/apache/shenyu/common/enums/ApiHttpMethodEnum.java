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

package org.apache.shenyu.common.enums;

import org.apache.shenyu.common.exception.ShenyuException;

import java.util.Arrays;

/**
 * the api http method type.
 */
public enum ApiHttpMethodEnum {

    /**
     * get.
     */
    GET("GET", 0),

    /**
     * head.
     */
    HEAD("HEAD", 1),

    /**
     * post.
     */
    POST("POST", 2),

    /**
     * put.
     */
    PUT("PUT", 3),

    /**
     * put.
     */
    PATCH("PATCH", 4),

    /**
     * delete.
     */
    DELETE("DELETE", 5),

    /**
     * options.
     */
    OPTIONS("OPTIONS", 6),

    /**
     * trace.
     */
    TRACE("TRACE", 7),

    /**
     * not_http.
     */
    NOT_HTTP("NOT_HTTP", 8);

    private final String name;

    private final Integer value;

    /**
     * Construct.
     *
     * @param name  name
     * @param value value
     */
    ApiHttpMethodEnum(final String name, final Integer value) {
        this.name = name;
        this.value = value;
    }

    /**
     * get name.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * get value.
     *
     * @return value
     */
    public Integer getValue() {
        return value;
    }

    /**
     * getValueByName.
     *
     * @param name name
     * @return value
     */
    public static Integer getValueByName(final String name) {
        return Arrays.stream(ApiHttpMethodEnum.values())
                .filter(e -> e.name.equals(name)).findFirst()
                .map(item -> item.value)
                .orElseThrow(() -> new ShenyuException(String.format(" this http method can not support %s", name)));
    }

    /**
     * build ApiHttpMethodEnum by name .
     *
     * @param name name
     * @return ApiHttpMethodEnum
     */
    public static ApiHttpMethodEnum of(final String name) {
        return Arrays.stream(ApiHttpMethodEnum.values())
                .filter(e -> e.name.equals(name)).findFirst()
                .orElseThrow(() -> new ShenyuException(String.format(" this http method can not support %s", name)));
    }

}
