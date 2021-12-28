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

package org.apache.shenyu.agent.core.enums;

/**
 * The enum Point type.
 */
public enum PointType {
    /**
     * Constructor point type.
     */
    CONSTRUCTOR("constructor"),
    
    /**
     * Instance method point type.
     */
    INSTANCE_METHOD("instanceMethod"),
    
    /**
     * Static method point type.
     */
    STATIC_METHOD("staticMethod");
    
    private final String name;
    
    PointType(final String name) {
        this.name = name;
    }
    
    /**
     * Gets name.
     *
     * @return the name
     */
    public String getName() {
        return name;
    }
}
