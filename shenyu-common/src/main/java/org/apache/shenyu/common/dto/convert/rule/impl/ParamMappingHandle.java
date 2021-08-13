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

import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * Param mapping handle.
 */
public class ParamMappingHandle implements RuleHandle {

    private Set<String> removeParameterKeys;

    private List<ParamMapInfo> replaceParameterKeys;

    private List<ParamMapInfo> addParameterKeys;

    /**
     * get removeParameterKeys.
     *
     * @return removeParameterKeys
     */
    public Set<String> getRemoveParameterKeys() {
        return removeParameterKeys;
    }

    /**
     * set removeParameterKeys.
     *
     * @param removeParameterKeys removeParameterKeys
     */
    public void setRemoveParameterKeys(final Set<String> removeParameterKeys) {
        this.removeParameterKeys = removeParameterKeys;
    }

    /**
     * get replaceParameterKeys.
     *
     * @return replaceParameterKeys
     */
    public List<ParamMapInfo> getReplaceParameterKeys() {
        return replaceParameterKeys;
    }

    /**
     * set replaceParameterKeys.
     *
     * @param replaceParameterKeys replaceParameterKeys
     */
    public void setReplaceParameterKeys(final List<ParamMapInfo> replaceParameterKeys) {
        this.replaceParameterKeys = replaceParameterKeys;
    }

    /**
     * get addParameterKeys.
     *
     * @return addParameterKeys
     */
    public List<ParamMapInfo> getAddParameterKeys() {
        return addParameterKeys;
    }

    /**
     * set addParameterKeys.
     *
     * @param addParameterKeys addParameterKeys
     */
    public void setAddParameterKeys(final List<ParamMapInfo> addParameterKeys) {
        this.addParameterKeys = addParameterKeys;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ParamMappingHandle that = (ParamMappingHandle) o;
        return Objects.equals(removeParameterKeys, that.removeParameterKeys) && Objects.equals(replaceParameterKeys, that.replaceParameterKeys)
                && Objects.equals(addParameterKeys, that.addParameterKeys);
    }

    @Override
    public int hashCode() {
        return Objects.hash(removeParameterKeys, replaceParameterKeys, addParameterKeys);
    }

    @Override
    public String toString() {
        return "ParamMappingHandle{"
                + "removeParameterKeys="
                + removeParameterKeys
                + ", replaceParameterKeys="
                + replaceParameterKeys
                + ", addParameterKeys="
                + addParameterKeys
                + '}';
    }

    @Override
    public RuleHandle createDefault(final String path) {
        return this;
    }

    public static class ParamMapInfo {

        private String path;

        private String key;

        private String value;

        /**
         * get path.
         *
         * @return path
         */
        public String getPath() {
            return path;
        }

        /**
         * set path.
         *
         * @param path path
         */
        public void setPath(final String path) {
            this.path = path;
        }

        /**
         * get key.
         *
         * @return key
         */
        public String getKey() {
            return key;
        }

        /**
         * set key.
         *
         * @param key key
         */
        public void setKey(final String key) {
            this.key = key;
        }

        /**
         * get value.
         *
         * @return value
         */
        public String getValue() {
            return value;
        }

        /**
         * set value.
         *
         * @param value value
         */
        public void setValue(final String value) {
            this.value = value;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            ParamMapInfo that = (ParamMapInfo) o;
            return Objects.equals(path, that.path) && Objects.equals(key, that.key) && Objects.equals(value, that.value);
        }

        @Override
        public int hashCode() {
            return Objects.hash(path, key, value);
        }

        @Override
        public String toString() {
            return "ParamMapInfo{"
                    + "path='"
                    + path
                    + '\''
                    + ", key='"
                    + key
                    + '\''
                    + ", value='"
                    + value
                    + '\''
                    + '}';
        }
    }
}
