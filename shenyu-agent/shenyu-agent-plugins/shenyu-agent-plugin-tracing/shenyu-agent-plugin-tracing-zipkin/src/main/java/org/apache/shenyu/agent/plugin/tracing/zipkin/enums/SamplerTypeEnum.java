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

package org.apache.shenyu.agent.plugin.tracing.zipkin.enums;

import java.util.Arrays;

/**
 * The enum Sampler type enum.
 */
public enum SamplerTypeEnum {
    
    /**
     * Cons sampler type enum.
     */
    CONS("const"),
    
    /**
     * Counting sampler type enum.
     */
    COUNTING("counting"),
    
    /**
     * Rate limiting sampler type enum.
     */
    RATE_LIMITING("ratelimiting"),
    
    /**
     * Boundary sampler type enum.
     */
    BOUNDARY("boundary"),
    
    /**
     * Always sample sampler type enum.
     */
    ALWAYS_SAMPLE("AlwaysSample");
    
    private final String name;
    
    SamplerTypeEnum(final String name) {
        this.name = name;
    }
    
    /**
     * get name.
     *
     * @return name name
     */
    public String getName() {
        return name;
    }
    
    
    /**
     * Gets enum by name.
     *
     * @param name the name
     * @return the enum by name
     */
    public static SamplerTypeEnum getEnumByName(final String name) {
        return Arrays.stream(SamplerTypeEnum.values())
                .filter(e -> e.getName().equals(name)).findFirst()
                .orElse(SamplerTypeEnum.ALWAYS_SAMPLE);
    }
}
