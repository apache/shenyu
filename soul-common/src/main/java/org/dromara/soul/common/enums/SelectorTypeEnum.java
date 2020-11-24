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

package org.dromara.soul.common.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * SelectorTypeEnum.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@RequiredArgsConstructor
@Getter
public enum SelectorTypeEnum {

    /**
     * full selector type enum.
     */
    FULL_FLOW(0, "全流量"),

    /**
     * Or match mode enum.
     */
    CUSTOM_FLOW(1, "自定义流量");

    private final int code;

    private final String name;

    /**
     * get selector type name by code.
     *
     * @param code selector type code.
     * @return selector type name.
     */
    public static String getSelectorTypeByCode(final int code) {
        for (SelectorTypeEnum selectorTypeEnum : SelectorTypeEnum.values()) {
            if (selectorTypeEnum.getCode() == code) {
                return selectorTypeEnum.getName();
            }
        }
        return null;
    }
}
