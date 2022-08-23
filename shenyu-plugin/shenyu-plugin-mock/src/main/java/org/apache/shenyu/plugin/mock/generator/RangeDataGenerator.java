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

import org.apache.shenyu.plugin.mock.util.RandomUtil;
import org.apache.shenyu.spi.Join;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Specify the in-list data generator.
 */
@Join
public class RangeDataGenerator implements Generator<String> {
    
    private List<String> data;
    
    @Override
    public String getName() {
        return "list";
    }
    
    @Override
    public String generate() {
        return data.get(RandomUtil.randomInt(0, data.size() - 1));
    }
    
    @Override
    public int getParamSize() {
        return 0;
    }
    
    @Override
    public void initParam(final List<String> params, final String rule) {
        String rangeData = params.get(0).replaceAll("\\[(.+)]", "$1");
        data = Arrays.stream(rangeData.split("(?<!\\\\),"))
                .map(data -> data.replace("\\,", ","))
                .collect(Collectors.toList());
    }
    
    @Override
    public boolean match(final String rule) {
        boolean matches = rule.matches("^list\\|\\[.+]$");
        if (matches) {
            String candidateData = rule.substring(6, rule.length() - 1);
            return !candidateData.matches("^\\s+$");
        }
        return false;
    }
    
    @Override
    public String[] getPrefixAndSuffix() {
        return new String[]{"\"", "\""};
    }
}
