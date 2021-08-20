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
 * MatchModeEnum.
 */
public enum MatchModeEnum {

    /**
     * And match mode enum.
     */
    AND(0, "and"),

    /**
     * Or match mode enum.
     */
    OR(1, "or");

    private final int code;

    private final String name;

    /**
     * all args constructor.
     *
     * @param code code
     * @param name name
     */
    MatchModeEnum(final int code, final String name) {
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
     * get match mode name by code.
     *
     * @param code match mode code.
     * @return match mode name.
     */
    public static String getMatchModeByCode(final int code) {
        return Arrays.stream(MatchModeEnum.values())
                .filter(e -> e.code == code).findFirst()
                .orElse(MatchModeEnum.AND)
                .getName();
    }
}
