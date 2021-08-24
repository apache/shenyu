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

package org.apache.shenyu.common.dto.convert.rule.impl;

import org.apache.shenyu.common.dto.convert.rule.RuleHandle;

/**
 * Cryptor response rule handle.
 */
public class CryptorResponseRuleHandle implements RuleHandle {

    private String strategyName;

    private String key;

    private String fieldNames;

    /**
     * get strategyName.
     * @return strategyName
     */
    public String getStrategyName() {
        return strategyName;
    }

    /**
     * set strategyName.
     * @param strategyName strategyName
     */
    public void setStrategyName(final String strategyName) {
        this.strategyName = strategyName;
    }

    /**
     * get key.
     * @return key
     */
    public String getKey() {
        return key;
    }

    /**
     * set key.
     * @param key key
     */
    public void setKey(final String key) {
        this.key = key;
    }

    /**
     * get fieldNames.
     * @return fieldNames
     */
    public String getFieldNames() {
        return fieldNames;
    }

    /**
     * set fieldNames.
     * @param fieldNames fieldNames
     */
    public void setFieldNames(final String fieldNames) {
        this.fieldNames = fieldNames;
    }

    @Override
    public String toString() {
        return "CryptorResponseRuleHandle{"
                + ", strategyName='" + strategyName + '\''
                + ", key='" + key + '\''
                + ", fieldNames='" + fieldNames + '\''
                + '}';
    }

    @Override
    public RuleHandle createDefault(final String path) {
        return this;
    }

}
