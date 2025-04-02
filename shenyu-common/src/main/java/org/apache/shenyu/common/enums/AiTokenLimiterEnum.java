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

/**
 * AiTokenLimiterEnum.
 */
public enum AiTokenLimiterEnum {
    /**
     * Ip key resolver enum.
     */
    IP("ip"),
    
    /**
     * Uri key resolver enum.
     */
    URI("uri"),
    
    /**
     * Header key resolver enum.
     */
    HEADER("header"),
    
    /**
     * Parameter key resolver enum.
     */
    PARAMETER("parameter"),
    
    /**
     * Cookie key resolver enum.
     */
    COOKIE("cookie"),
    
    /**
     * contextPath key resolver enum.
     */
    CONTEXT_PATH("contextPath");
    
    private final String name;
    
    /**
     * all args constructor.
     *
     * @param name name
     */
    AiTokenLimiterEnum(final String name) {
        this.name = name;
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
     * get key resolver name by code.
     *
     * @param keyResolverName key resolver name.
     * @return key resolver name.
     */
    public static AiTokenLimiterEnum getByName(final String keyResolverName) {
        for (AiTokenLimiterEnum value : AiTokenLimiterEnum.values()) {
            if (value.getName().equals(keyResolverName)) {
                return value;
            }
        }
        return CONTEXT_PATH;
    }
}
