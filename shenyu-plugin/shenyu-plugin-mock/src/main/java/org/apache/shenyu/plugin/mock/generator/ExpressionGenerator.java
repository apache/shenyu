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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;

import java.util.List;

public class ExpressionGenerator implements Generator<String> {

    private static final Logger LOG = LoggerFactory.getLogger(ExpressionGenerator.class);

    private String expression;

    private final ExpressionParser parser = new SpelExpressionParser();

    @Override
    public String getName() {
        return "expression";
    }

    @Override
    public String generate() {
        String val;
        try {
            val = "" + parser.parseExpression(expression).getValue();
        } catch (Exception e) {
            val = "Wrong expression!!!,please check!!!";
            LOG.error(e.getMessage(), e);
        }
        return val;
    }

    @Override
    public int getParamSize() {
        return 1;
    }

    @Override
    public void initParam(final List<String> params, final String rule) {
        expression = params.get(0);
    }

    @Override
    public boolean match(final String rule) {
        return rule.matches("^expression\\|.+");
    }
}
