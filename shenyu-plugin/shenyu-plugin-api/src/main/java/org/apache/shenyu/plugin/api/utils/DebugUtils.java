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

package org.apache.shenyu.plugin.api.utils;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpHeaders;
import org.springframework.web.server.ServerWebExchange;

import java.util.Optional;

/**
 * Writing some debugging information into the response header,
 * which is very effective in troubleshooting problems.
 */
public class DebugUtils {
    
    private static final String ENABLE = "enable";
    
    /**
     * write debug headers.
     *
     * @param exchange the exchange
     */
    public static void write(final ServerWebExchange exchange) {
        final Environment env = SpringBeanUtils.getInstance().getBean(Environment.class);
        //here, you can combine Nacos for dynamic configuration
        final Boolean enable = env.getProperty("shenyu.debug.enable", boolean.class, false);
        if (!enable) {
            //gateway not enable debug
            return;
        }
        final String debug = exchange.getRequest().getHeaders().getFirst(Constants.HEADER_DEBUG);
        if (!Boolean.TRUE.toString().equals(debug) && !ENABLE.equals(debug)) {
            //not have debug header, or the value is wrong.
            return;
        }
        final GsonUtils instance = GsonUtils.getInstance();
        final HttpHeaders responseHeaders = exchange.getResponse().getHeaders();
        Optional.ofNullable((MetaData) exchange.getAttribute(Constants.META_DATA))
                .ifPresent(metaData -> responseHeaders.add(Constants.META_DATA, instance.toJson(metaData)));
        Optional.ofNullable((SelectorData) exchange.getAttribute(Constants.SELECTOR))
                .ifPresent(selectorData -> responseHeaders.add(Constants.SELECTOR, instance.toJson(selectorData)));
        Optional.ofNullable((RuleData) exchange.getAttribute(Constants.RULE))
                .ifPresent(ruleData -> responseHeaders.add(Constants.RULE, instance.toJson(ruleData)));
    }
}
