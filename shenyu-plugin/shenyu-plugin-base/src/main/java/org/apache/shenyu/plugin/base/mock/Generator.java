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

package org.apache.shenyu.plugin.base.mock;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.shenyu.spi.SPI;

/**
 * Abstract generator.
 */
@SPI
public interface Generator<T> {
    
    /**
     * rule name.
     *
     * @return name
     */
    String getName();
    
    /**
     * generate mock data.
     *
     * @return random data
     */
    T generate();
    
    /**
     * get size of rule params.
     *
     * @return params size
     */
    int getParamSize();
    
    /**
     * init generator.
     *
     * @param rule rule
     */
    default void parseRule(final String rule) {
        List<String> params = new ArrayList<>();
        String[] split = rule.split("(?<!\\\\)\\|");
        if (split.length >= getParamSize() + 1) {
            params.addAll(Arrays.stream(split)
                .map(p -> p.replaceAll("\\\\\\|", "|"))
                .skip(1)
                .collect(Collectors.toList()));
        }
        initParam(params);
    }
    
    /**
     * param from rule.
     *
     * @param params rule params.
     */
    default void initParam(List<String> params) {
    
    }
    
    /**
     * Determine whether the rule meets the format requirements.
     *
     * @param rule rule
     * @return if match return true.
     */
    boolean match(String rule);
    
}
