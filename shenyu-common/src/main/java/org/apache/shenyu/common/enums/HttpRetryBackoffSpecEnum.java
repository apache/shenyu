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

import java.util.Arrays;

/**
 * The http scheme enum.
 */
public enum HttpRetryBackoffSpecEnum {

    /**
     * Default retry.
     */
    DEFAULT_BACKOFF("default"),

    /**
     * Fixed retry.
     */
    FIXED_BACKOFF("fixed"),

    /**
     * Exponential retry.
     */
    EXPONENTIAL_BACKOFF("exponential"),

    /**
     * Custom retry.
     */
    CUSTOM_BACKOFF("custom");

    private final String name;


    HttpRetryBackoffSpecEnum(final String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static HttpRetryBackoffSpecEnum acquireByName(final String name) {
        return Arrays.stream(HttpRetryBackoffSpecEnum.values())
                .filter(e -> e.getName().equals(name)).findFirst()
                .orElse(HttpRetryBackoffSpecEnum.DEFAULT_BACKOFF);
    }

    public static String getDefault() {
        return DEFAULT_BACKOFF.getName();
    }

}
