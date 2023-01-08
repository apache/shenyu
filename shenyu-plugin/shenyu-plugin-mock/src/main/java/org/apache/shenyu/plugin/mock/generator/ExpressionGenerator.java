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

import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.mock.api.MockRequest;
import org.apache.shenyu.plugin.mock.util.MockUtil;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.expression.MapAccessor;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;

import java.util.List;

@Join
public class ExpressionGenerator implements Generator<String> {

    private static final Logger LOG = LoggerFactory.getLogger(ExpressionGenerator.class);

    private static final ExpressionParser PARSER = new SpelExpressionParser();

    private static final EvaluationContext CONTEXT = initContext();

    @Override
    public String getName() {
        return "expression";
    }

    @Override
    public String doGenerate(final List<String> params, final String rule, final MockRequest mockRequest) {

        String expression = params.get(0);

        CONTEXT.setVariable("req", mockRequest);
        Object val = PARSER.parseExpression(expression).getValue(CONTEXT);
        return JsonUtils.toJson(val);
    }

    @Override
    public int getParamSize() {
        return 1;
    }

    @Override
    public boolean match(final String rule) {
        return rule.matches("^expression\\|.+");
    }

    private static EvaluationContext initContext() {

        StandardEvaluationContext context = new StandardEvaluationContext();

        try {
            registerMockFunction(context, "double", "randomDouble", double.class, double.class, String[].class);

            registerMockFunction(context, "bool", "bool");

            registerMockFunction(context, "int", "randomInt", int.class, int.class);

            registerMockFunction(context, "email", "email");

            registerMockFunction(context, "phone", "phone");

            registerMockFunction(context, "zh", "zh", int.class, int.class);

            registerMockFunction(context, "en", "en", int.class, int.class);

            registerMockFunction(context, "oneOf", "oneOf", Object[].class);

            registerMockFunction(context, "current", "current", String[].class);

            registerMockFunction(context, "array", "array", Object.class, int.class);

            context.addPropertyAccessor(new MapAccessor());

        } catch (NoSuchMethodException e) {
            // It will never happen
            LOG.error(e.getMessage(), e);
        }
        return context;
    }

    private static void registerMockFunction(final StandardEvaluationContext context,
                                             final String name,
                                             final String methodName,
                                             final Class<?>... parameterTypes) throws NoSuchMethodException {
        context.registerFunction(name,
                MockUtil.class.getDeclaredMethod(methodName, parameterTypes));
    }
}
