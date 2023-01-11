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

import org.apache.shenyu.plugin.mock.api.MockRequest;
import org.apache.shenyu.plugin.mock.util.MockUtil;
import org.apache.shenyu.spi.Join;

import java.util.List;

/**
 * Random int value generator in the specified range.
 */
@Join
public class RandomIntGenerator implements Generator<Integer> {

    @Override
    public String getName() {
        return "int";
    }

    @Override
    public Integer doGenerate(final List<String> params, final String rule, final MockRequest mockRequest) {
        String[] range = params.get(0).split("-");
        int min = Integer.parseInt(range[0]);
        int max = Integer.parseInt(range[1]);
        return MockUtil.randomInt(min, max);
    }

    @Override
    public int getParamSize() {
        return 1;
    }

    @Override
    public boolean match(final String rule) {
        return rule.matches("^int\\|\\d+-\\d+$");
    }

}
