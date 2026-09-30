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

package org.apache.shenyu.plugin.logging.console.entity;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.plugin.logging.common.entity.CommonLoggingRuleHandle;
import org.apache.shenyu.plugin.logging.desensitize.api.enums.DataDesensitizeEnum;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;

/**
 * Immutable logging console rule configuration.
 */
public final class LoggingConsoleRuleHandle {

    private final boolean desensitized;

    private final String keyword;

    private final String dataDesensitizeAlg;

    private final KeyWordMatch keyWordMatch;

    /**
     * Create a logging console rule configuration.
     *
     * @param ruleHandle common logging rule configuration
     */
    public LoggingConsoleRuleHandle(final CommonLoggingRuleHandle ruleHandle) {
        this.keyword = ruleHandle.getKeyword();
        this.desensitized = StringUtils.isNotBlank(keyword) && Boolean.TRUE.equals(ruleHandle.getMaskStatus());
        this.dataDesensitizeAlg = Objects.nonNull(ruleHandle.getMaskType())
                ? ruleHandle.getMaskType() : DataDesensitizeEnum.MD5_ENCRYPT.getDataDesensitizeAlg();
        this.keyWordMatch = new KeyWordMatch(desensitized
                ? new HashSet<>(Arrays.asList(keyword.split(";"))) : Collections.emptySet());
    }

    /**
     * Whether desensitization is enabled.
     *
     * @return true when enabled
     */
    public boolean isDesensitized() {
        return desensitized;
    }

    /**
     * Get configured keywords.
     *
     * @return configured keywords
     */
    public String getKeyword() {
        return keyword;
    }

    /**
     * Get the desensitization algorithm.
     *
     * @return desensitization algorithm
     */
    public String getDataDesensitizeAlg() {
        return dataDesensitizeAlg;
    }

    /**
     * Get the compiled keyword matcher.
     *
     * @return compiled keyword matcher
     */
    public KeyWordMatch getKeyWordMatch() {
        return keyWordMatch;
    }
}
