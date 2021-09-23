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
import org.springframework.http.HttpStatus;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * The type ModifyResponse rule handle.
 */
public class ModifyResponseRuleHandle implements RuleHandle {

    private static final long serialVersionUID = -3863724068338111444L;

    /**
     * add header map.
     */
    private Map<String, String> addHeaders;

    /**
     * set header map.
     */
    private Map<String, String> setHeaders;

    /**
     * replace header map
     * key: oldHeaderKey, value: newHeaderKey.
     */
    private Map<String, String> replaceHeaderKeys;

    /**
     * remove header List.
     */
    private Set<String> removeHeaderKeys;

    /**
     * http response status code.
     */
    private int statusCode;

    /**
     * add body List.
     */
    private List<ParamMappingHandle.ParamMapInfo> addBodyKeys;

    /**
     * replace body List.
     */
    private List<ParamMappingHandle.ParamMapInfo> replaceBodyKeys;

    /**
     * remove body List.
     */
    private Set<String> removeBodyKeys;

    /**
     * get serialVersionUID.
     *
     * @return serialVersionUID
     */
    public static long getSerialVersionUID() {
        return serialVersionUID;
    }

    /**
     * get addHeaders.
     *
     * @return addHeaders
     */
    public Map<String, String> getAddHeaders() {
        return addHeaders;
    }

    /**
     * set addHeaders.
     *
     * @param addHeaders addHeaders
     */
    public void setAddHeaders(final Map<String, String> addHeaders) {
        this.addHeaders = addHeaders;
    }

    /**
     * get setHeaders.
     *
     * @return setHeaders
     */
    public Map<String, String> getSetHeaders() {
        return setHeaders;
    }

    /**
     * set setHeaders.
     *
     * @param setHeaders setHeaders
     */
    public void setSetHeaders(final Map<String, String> setHeaders) {
        this.setHeaders = setHeaders;
    }

    /**
     * get replaceHeaderKeys.
     *
     * @return replaceHeaderKeys
     */
    public Map<String, String> getReplaceHeaderKeys() {
        return replaceHeaderKeys;
    }

    /**
     * set replaceHeaderKeys.
     *
     * @param replaceHeaderKeys replaceHeaderKeys
     */
    public void setReplaceHeaderKeys(final Map<String, String> replaceHeaderKeys) {
        this.replaceHeaderKeys = replaceHeaderKeys;
    }

    /**
     * get removeHeaderKeys.
     *
     * @return removeHeaderKeys
     */
    public Set<String> getRemoveHeaderKeys() {
        return removeHeaderKeys;
    }

    /**
     * set removeHeaderKeys.
     *
     * @param removeHeaderKeys removeHeaderKeys
     */
    public void setRemoveHeaderKeys(final Set<String> removeHeaderKeys) {
        this.removeHeaderKeys = removeHeaderKeys;
    }

    /**
     * get statusCode.
     *
     * @return statusCode
     */
    public int getStatusCode() {
        return statusCode;
    }

    /**
     * set statusCode.
     *
     * @param statusCode statusCode
     */
    public void setStatusCode(final int statusCode) {
        this.statusCode = statusCode;
    }

    /**
     * get addBodyKeys.
     *
     * @return addBodyKeys
     */
    public List<ParamMappingHandle.ParamMapInfo> getAddBodyKeys() {
        return addBodyKeys;
    }

    /**
     * set addBodyKeys.
     *
     * @param addBodyKeys addBodyKeys
     */
    public void setAddBodyKeys(final List<ParamMappingHandle.ParamMapInfo> addBodyKeys) {
        this.addBodyKeys = addBodyKeys;
    }

    /**
     * get replaceBodyKeys.
     *
     * @return replaceBodyKeys
     */
    public List<ParamMappingHandle.ParamMapInfo> getReplaceBodyKeys() {
        return replaceBodyKeys;
    }

    /**
     * set replaceBodyKeys.
     *
     * @param replaceBodyKeys replaceBodyKeys
     */
    public void setReplaceBodyKeys(final List<ParamMappingHandle.ParamMapInfo> replaceBodyKeys) {
        this.replaceBodyKeys = replaceBodyKeys;
    }

    /**
     * get removeBodyKeys.
     *
     * @return removeBodyKeys
     */
    public Set<String> getRemoveBodyKeys() {
        return removeBodyKeys;
    }

    /**
     * set removeBodyKeys.
     *
     * @param removeBodyKeys removeBodyKeys
     */
    public void setRemoveBodyKeys(final Set<String> removeBodyKeys) {
        this.removeBodyKeys = removeBodyKeys;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ModifyResponseRuleHandle that = (ModifyResponseRuleHandle) o;
        return statusCode == that.statusCode && Objects.equals(addHeaders, that.addHeaders)
                && Objects.equals(setHeaders, that.setHeaders) && Objects.equals(replaceHeaderKeys, that.replaceHeaderKeys)
                && Objects.equals(removeHeaderKeys, that.removeHeaderKeys) && Objects.equals(addBodyKeys, that.addBodyKeys)
                && Objects.equals(replaceBodyKeys, that.replaceBodyKeys) && Objects.equals(removeBodyKeys, that.removeBodyKeys);
    }

    @Override
    public int hashCode() {
        return Objects.hash(addHeaders, setHeaders, replaceHeaderKeys, removeHeaderKeys, statusCode, addBodyKeys, replaceBodyKeys, removeBodyKeys);
    }

    @Override
    public String toString() {
        return "ModifyResponseRuleHandle{"
                + "addHeaders="
                + addHeaders
                + ", setHeaders="
                + setHeaders
                + ", replaceHeaderKeys="
                + replaceHeaderKeys
                + ", removeHeaderKeys="
                + removeHeaderKeys
                + ", statusCode="
                + statusCode
                + ", addBodyKeys="
                + addBodyKeys
                + ", replaceBodyKeys="
                + replaceBodyKeys
                + ", removeBodyKeys="
                + removeBodyKeys
                + '}';
    }

    @Override
    public RuleHandle createDefault(final String path, final String rpcExt) {
        this.statusCode = HttpStatus.OK.value();
        return this;
    }
}
