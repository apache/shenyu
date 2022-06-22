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

package org.apache.shenyu.plugin.mock.generator;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Abstract generator.
 */
public abstract class AbstractGenerator<T> {
    
    private List<String> params;
    
    /**
     * get params.
     *
     * @return params
     */
    public List<String> getParams() {
        return params;
    }
    
    /**
     * rule name.
     *
     * @return name
     */
    abstract String getName();
    
    /**
     * generate mock data.
     *
     * @return random data
     */
    public abstract T generate();
    
    /**
     * get size of rule params.
     *
     * @return params size
     */
    abstract int getParamSize();
    
    /**
     * init generator.
     *
     * @param rule rule
     */
    public void parseRule(final String rule) {
        String[] split = rule.split("\\|");
        if (split.length >= getParamSize() + 1) {
            params = Arrays.stream(split).skip(1).collect(Collectors.toList());
        }
        initParam();
    }
    
    /**
     * param from rule.
     */
    abstract void initParam();
    
    /**
     * Determine whether the rule meets the format requirements.
     *
     * @param rule rule
     * @return if match return true.
     */
    public abstract boolean match(String rule);
    
}
