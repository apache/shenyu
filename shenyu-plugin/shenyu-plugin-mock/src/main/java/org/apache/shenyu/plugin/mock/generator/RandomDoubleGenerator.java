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
import java.util.Objects;

/**
 * Random double value generator in the specified range.
 */
@Join
public class RandomDoubleGenerator implements Generator<String> {

    @Override
    public String getName() {
        return "double";
    }

    @Override
    public String doGenerate(final List<String> params, final String rule, final MockRequest mockRequest) {
        String[] range = params.get(0).split("-");
        double min = Double.parseDouble(range[0]);
        double max = Double.parseDouble(range[1]);
        String format = null;
        if (params.size() == 2) {
            format = Objects.equals(params.get(1), "") ? null : params.get(1);
        }
        return MockUtil.randomDouble(min, max, format).toString();
    }

    @Override
    public int getParamSize() {
        return 1;
    }

    @Override
    public boolean match(final String rule) {
        return rule.matches("^double\\|\\d+(?:\\.\\d+)?-\\d+(?:\\.\\d+)?.*");
    }

    @Override
    public String[] getPrefixAndSuffix() {
        return new String[]{"\"", "\""};
    }
}

