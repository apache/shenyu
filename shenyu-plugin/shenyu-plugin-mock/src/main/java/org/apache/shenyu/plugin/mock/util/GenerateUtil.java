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

package org.apache.shenyu.plugin.mock.util;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.shenyu.plugin.mock.generator.AbstractGenerator;
import org.apache.shenyu.plugin.mock.generator.BoolGenerator;
import org.apache.shenyu.plugin.mock.generator.CnameGenerator;
import org.apache.shenyu.plugin.mock.generator.CurrentTimeGenerator;
import org.apache.shenyu.plugin.mock.generator.EmailGenerator;
import org.apache.shenyu.plugin.mock.generator.EnStringGenerator;
import org.apache.shenyu.plugin.mock.generator.IdCardNumGenerator;
import org.apache.shenyu.plugin.mock.generator.PhoneGenerator;
import org.apache.shenyu.plugin.mock.generator.RandomDoubleGenerator;
import org.apache.shenyu.plugin.mock.generator.RandomIntGenerator;
import org.apache.shenyu.plugin.mock.generator.RangeDataGenerator;
import org.apache.shenyu.plugin.mock.generator.ZhStringGenerator;

/**
 * GenerateUtil.
 */
public class GenerateUtil {
    
    /**
     * map of generator.
     */
    private static final Map<String, AbstractGenerator<?>> GENERATORS = new HashMap<>();
    
    /**
     * Regular expression to extract placeholders.
     */
    private static final Pattern RULE_PATTERN = Pattern
        .compile("(\\$\\{.+?})", Pattern.MULTILINE);
    
    /**
     * Regular expression to extract rule content.
     */
    private static final Pattern RULE_CONTENT_PATTERN = Pattern
        .compile("^\\$\\{(.+?)}$", Pattern.MULTILINE);
    
    static {
        GENERATORS.put("int", new RandomIntGenerator());
        GENERATORS.put("zh", new ZhStringGenerator());
        GENERATORS.put("en", new EnStringGenerator());
        GENERATORS.put("cname", new CnameGenerator());
        GENERATORS.put("double", new RandomDoubleGenerator());
        GENERATORS.put("bool", new BoolGenerator());
        GENERATORS.put("phone", new PhoneGenerator());
        GENERATORS.put("current", new CurrentTimeGenerator());
        GENERATORS.put("idcard", new IdCardNumGenerator());
        GENERATORS.put("email", new EmailGenerator());
        GENERATORS.put("list", new RangeDataGenerator());
    }
    
    private static Object generate(final String rule) {
        final Matcher matcher = RULE_CONTENT_PATTERN.matcher(rule);
        if (matcher.find()) {
            String ruleContent = matcher.group(1);
            String ruleName = ruleContent.split("\\|")[0];
            AbstractGenerator<?> generator = GENERATORS.get(ruleName);
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
    
    /**
     * add data generator.
     * @param name name
     * @param generator generator
     */
    public static void addGenerator(final String name, final AbstractGenerator<?> generator) {
        GENERATORS.put(name, generator);
    }
    
}



