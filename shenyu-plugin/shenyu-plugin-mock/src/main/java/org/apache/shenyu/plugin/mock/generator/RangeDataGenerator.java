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

import java.util.Arrays;
import java.util.List;

/**
 * Specify the in-list data generator.
 */
@Join
public class RangeDataGenerator implements Generator<String> {

    @Override
    public String getName() {
        return "list";
    }

    @Override
    public String doGenerate(final List<String> params, final String rule, final MockRequest mockRequest) {
        String rangeData = params.get(0).replaceAll("\\[(.+)]", "$1");
        String[] data = Arrays.stream(rangeData.split("(?<!\\\\),"))
                .map(_data -> _data.replace("\\,", ","))
                .toArray(String[]::new);

        return MockUtil.oneOf(data).toString();
    }

    @Override
    public int getParamSize() {
        return 0;
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
