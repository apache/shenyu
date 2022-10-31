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
     * mask keyword.
     */
    private String keyword;

    /**
     * mask type, include md5 and character replacement.
     */
    private String maskType;

    /**
     * mask status, include true and false.
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
     * get mask type.
     *
     * @return mask type
     */
    public String getMaskType() {
        return maskType;
    }

    /**
     * set mask type.
     *
     * @param maskType mask type
     */
    public void setMaskType(final String maskType) {
        this.maskType = maskType;
    }

    /**
     * get mask status.
     *
     * @return mask status
     */
    public Boolean getMaskStatus() {
        return maskStatus;
    }

    /**
     * set mask status.
     *
     * @param maskStatus mask status
     */
    public void setMaskStatus(final Boolean maskStatus) {
        this.maskStatus = maskStatus;
    }
}
