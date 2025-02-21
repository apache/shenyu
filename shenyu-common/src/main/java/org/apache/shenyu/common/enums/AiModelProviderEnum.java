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
 * The Ai model provider enum.
 */
public enum AiModelProviderEnum {
    
    /**
     * OpenAI.
     */
    OPEN_AI("OpenAI"),
    
    /**
     * DeepSeek.
     */
    DEEP_SEEK("DeepSeek"),
    
    /**
     * ALiYun.
     */
    ALIYUN("ALiYun"),
    
    /**
     * OpenAPI.
     */
    OPEN_API("OpenAPI"),
    
    /**
     * Moonshot.
     */
    MOONSHOT("Moonshot");

    private final String name;

    AiModelProviderEnum(final String name) {
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
     * get model by name.
     *
     * @param name name
     * @return AiModelProviderEnum
     */
    public static AiModelProviderEnum getByName(final String name) {
        for (AiModelProviderEnum value : AiModelProviderEnum.values()) {
            if (value.getName().equals(name)) {
                return value;
            }
        }
        return null;
    }
}
