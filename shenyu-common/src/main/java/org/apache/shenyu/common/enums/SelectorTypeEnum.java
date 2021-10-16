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
 * SelectorTypeEnum.
 */
public enum SelectorTypeEnum {

    /**
     * full selector type enum.
     */
    FULL_FLOW(0, "full"),

    /**
     * Or match mode enum.
     */
    CUSTOM_FLOW(1, "custom");

    private final int code;

    private final String name;

    /**
     * all args constructor.
     *
     * @param code code
     * @param name name
     */
    SelectorTypeEnum(final int code, final String name) {
        this.code = code;
        this.name = name;
    }

    /**
     * get code.
     *
     * @return code
     */
    public int getCode() {
        return code;
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
     * get selector type name by code.
     *
     * @param code selector type code.
     * @return selector type name.
     */
    public static String getSelectorTypeByCode(final int code) {
        return Arrays.stream(SelectorTypeEnum.values())
                .filter(v -> v.getCode() == code)
                .findFirst()
                .map(SelectorTypeEnum::getName).orElse(null);
    }
}
