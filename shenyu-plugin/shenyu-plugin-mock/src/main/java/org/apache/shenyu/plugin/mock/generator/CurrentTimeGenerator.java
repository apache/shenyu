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

import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

/**
 * Current time generator.
 */
@Join
public class CurrentTimeGenerator implements Generator<String> {
    
    private static final String DEFAULT_FORMAT = "YYYY-MM-dd HH:mm:ss";
    
    private static final Logger LOG = LoggerFactory.getLogger(CurrentTimeGenerator.class);
    
    private String format;
    
    @Override
    public String getName() {
        return "current";
    }
    
    @Override
    public String generate() {
        LocalDateTime now = LocalDateTime.now();
        try {
            return DateTimeFormatter.ofPattern(format).format(now);
        } catch (DateTimeException e) {
            LOG.warn("format fail,use default format :{}", DEFAULT_FORMAT);
            return DateTimeFormatter.ofPattern(DEFAULT_FORMAT).format(now);
        }
        
    }
    
    @Override
    public int getParamSize() {
        return 0;
    }
    
    @Override
    public void initParam(final List<String> params, final String rule) {
        if (params.size() >= 1) {
            format = params.get(0);
        } else {
            format = DEFAULT_FORMAT;
        }
    }
    
    @Override
    public boolean match(final String rule) {
        return rule.matches("^current(\\|.+)?");
    }
    
    @Override
    public String[] getPrefixAndSuffix() {
        return new String[]{"\"", "\""};
    }
}
