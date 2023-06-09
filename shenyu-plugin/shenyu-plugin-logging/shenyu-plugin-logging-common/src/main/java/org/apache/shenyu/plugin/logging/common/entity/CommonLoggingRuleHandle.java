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

package org.apache.shenyu.plugin.logging.common.entity;

import org.apache.shenyu.common.dto.convert.rule.RuleHandle;

/**
 * common logging rule handle.
 */
public class CommonLoggingRuleHandle implements RuleHandle {

    /**
     * desensitize keyword.
     */
    private String keyword;

    /**
     * desensitize type, include md5 and character replacement.
     */
    private String maskType;

    /**
     * desensitize status, include true and false.
     */
    private Boolean maskStatus;

    /**
     * get keyword.
     *
     * @return keyword
     */
    public String getKeyword() {
        return keyword;
    }

    /**
     * set keyword.
     * @param keyword keyword
     */
    public void setKeyword(final String keyword) {
        this.keyword = keyword;
    }

    /**
     * get desensitize type.
     *
     * @return desensitize type
     */
    public String getMaskType() {
        return maskType;
    }

    /**
     * set desensitize type.
     *
     * @param desensitizeType desensitize type
     */
    public void setMaskType(final String desensitizeType) {
        this.maskType = maskType;
    }

    /**
     * get desensitize status.
     *
     * @return desensitize status
     */
    public Boolean getMaskStatus() {
        return maskStatus;
    }

    /**
     * set desensitize status.
     *
     * @param desensitizeStatus desensitize status
     */
    public void setMaskStatus(final Boolean desensitizeStatus) {
        this.maskStatus = desensitizeStatus;
    }
}
