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
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * GeneratorFactory.
 */
public final class GeneratorFactory {

    private static final Logger LOG = LoggerFactory.getLogger(GeneratorFactory.class);

    /**
     * If expression parsing fails, the ${} placeholder
     * will be replaced with the following.
     */
    private static final String ERROR_PARSE = "\"[#ERROR EXPRESSION#]\"";

    /**
     * Regular expression to extract rule content.
     */
    private static final Pattern RULE_CONTENT_PATTERN = Pattern
            .compile("^\\$\\{(.+?)}$", Pattern.MULTILINE);

    private GeneratorFactory() {
    }

    /**
     * New instance mock data generator.
     *
     * @param ruleName rule name
     * @param rule     full rule content
     * @return generator
     */
    public static Generator<?> newInstance(final String ruleName, final String rule) {
        try {
            return ExtensionLoader.getExtensionLoader(Generator.class).getJoin(ruleName);
        } catch (IllegalArgumentException e) {
            LOG.warn("{} can not parse,please check", rule);
        }
        return null;
    }

    private static String generate(final String rule, final MockRequest mockRequest) {
        final Matcher matcher = RULE_CONTENT_PATTERN.matcher(rule);
        if (!matcher.find()) {
            return rule;
        }

        String ruleContent = matcher.group(1);
        String ruleName = ruleContent.split("\\|")[0];
        Generator<?> generator = newInstance(ruleName, rule);
        if (generator == null || !generator.match(ruleContent)) {
            return rule;
        }

        String[] prefixAndSuffix = generator.getPrefixAndSuffix();
        try {
            Object generateData = generator.generate(ruleContent, mockRequest);
            return String.join("", prefixAndSuffix[0], generateData.toString(), prefixAndSuffix[1]);
        } catch (Exception e) {
            LOG.error(e.getMessage(), e);
            return rule;
        }
    }

    /**
     * replace placeholder in content.
     *
     * @param content response content
     * @param mockRequest request
     * @return replace the content after the placeholder.
     */
    public static String dealRule(final String content, final MockRequest mockRequest) {
        String afterDeal = content;
        String placeHolder = getPlaceholder(content);
        while (placeHolder != null) {
            Object generateData = generate(placeHolder, mockRequest);
            if (Objects.equals(generateData, placeHolder)) {
                generateData = ERROR_PARSE;
            }
            String toString = String.valueOf(generateData);
            placeHolder = placeHolder.replaceAll("([$|{}()\\]\\[])", "\\\\$1");
            afterDeal = afterDeal.replaceFirst(placeHolder, toString);
            placeHolder = getPlaceholder(afterDeal);
        }
        return afterDeal;
    }

    private static String getPlaceholder(final String rule) {
        int start = rule.indexOf("${");
        if (start < 0) {
            return null;
        }
        int counter = 1;
        for (int i = start + 2; i < rule.length(); i++) {
            char c = rule.charAt(i);
            if (c == '{') {
                counter++;
            } else if (c == '}') {
                counter--;
                if (counter == 0) {
                    return "${" + rule.substring(start + 2, i) + "}";
                }
            }
        }
        return null;
    }

}
