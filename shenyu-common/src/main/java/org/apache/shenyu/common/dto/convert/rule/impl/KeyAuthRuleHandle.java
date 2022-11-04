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

import java.util.Objects;

/**
 * Key-Auth rule handle.
 */
public class KeyAuthRuleHandle implements RuleHandle {

    /**
     * The key attribute name. It is required.
     */
    private String keyName;

    /**
     * The key value. It is required.
     */
    private String key;

    /**
     * Get key attribute name.
     *
     * @return keyName
     */
    public String getKeyName() {
        return keyName;
    }

    /**
     * Set key attribute name.
     *
     * @param keyName keyName
     */
    public void setKeyName(final String keyName) {
        this.keyName = keyName;
    }

    /**
     * Get the key.
     *
     * @return key
     */
    public String getKey() {
        return key;
    }

    /**
     * Set the key.
     *
     * @param key key
     */
    public void setKey(final String key) {
        this.key = key;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        KeyAuthRuleHandle that = (KeyAuthRuleHandle) o;
        return Objects.equals(keyName, that.keyName)
                && Objects.equals(key, that.key);
    }

    @Override
    public int hashCode() {
        return Objects.hash(keyName, key);
    }

    @Override
    public String toString() {
        return "KeyAuthRuleHandle{"
                + "keyName='" + keyName
                + "', key='" + key
                + "'}";
    }
}
