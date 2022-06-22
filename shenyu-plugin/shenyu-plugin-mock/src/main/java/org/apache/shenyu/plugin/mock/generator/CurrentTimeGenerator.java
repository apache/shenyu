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

import java.text.SimpleDateFormat;
import java.util.Date;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * current time generator.
 */
public class CurrentTimeGenerator extends AbstractGenerator<String> {
    
    private static final String DEFAULT_FORMAT = "YYYY-MM-dd HH:mm:ss";
    
    private static final Logger LOG = LoggerFactory.getLogger(CurrentTimeGenerator.class);
    
    private String format;
    
    @Override
    public String getName() {
        return "current";
    }
    
    @Override
    public String generate() {
        Date now = new Date();
        try {
            return new SimpleDateFormat(this.format).format(now);
        } catch (IllegalArgumentException e) {
            LOG.warn("illegal format: {} ,use default format :{}", format, DEFAULT_FORMAT);
            return new SimpleDateFormat(DEFAULT_FORMAT).format(now);
        }
        
    }
    
    @Override
    public int getParamSize() {
        return 0;
    }
    
    @Override
    void initParam() {
        if (super.getParams().size() >= 1) {
            format = super.getParams().get(0);
        } else {
            format = DEFAULT_FORMAT;
        }
    }
    
    @Override
    public boolean match(final String rule) {
        return rule.matches("^current(\\|.+)?");
    }
}
