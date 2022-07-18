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
    
    /**
     * Regular expression to extract placeholders.
     */
    private static final Pattern RULE_PATTERN = Pattern
        .compile("(\\$\\{.+?})", Pattern.MULTILINE);
    
    private static final Logger LOG = LoggerFactory.getLogger(GeneratorFactory.class);
    
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
    
    private static Object generate(final String rule) {
        final Matcher matcher = RULE_CONTENT_PATTERN.matcher(rule);
        if (matcher.find()) {
            String ruleContent = matcher.group(1);
            String ruleName = ruleContent.split("\\|")[0];
            Generator<?> generator = newInstance(ruleName, rule);
            if (generator == null || !generator.match(ruleContent)) {
                return rule;
            }
            generator.parseRule(ruleContent);
            return generator.generate();
        } else {
            return rule;
        }
    }
    
    /**
     * replace placeholder in content.
     *
     * @param content response content.
     * @return replace the content after the placeholder.
     */
    public static String dealRule(final String content) {
        String afterDeal = content;
        final Matcher matcher = RULE_PATTERN.matcher(afterDeal);
        while (matcher.find()) {
            String placeHolder = matcher.group(0);
            Object generateData = generate(placeHolder);
            if (Objects.equals(generateData, placeHolder)) {
                continue;
            }
            String toString = String.valueOf(generateData);
            if (generateData instanceof String) {
                toString = String.format("\"%s\"", toString);
            }
            placeHolder = placeHolder.replaceAll("([$|{}\\]\\[])", "\\\\$1");
            afterDeal = afterDeal.replaceFirst(placeHolder, toString);
        }
        return afterDeal;
    }
}
