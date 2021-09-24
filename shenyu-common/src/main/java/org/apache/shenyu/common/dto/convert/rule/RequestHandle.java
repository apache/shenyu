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

package org.apache.shenyu.common.dto.convert.rule;

import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.common.utils.CollectionUtils;

import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * this is RequestHandle plugin handle.
 */
public class RequestHandle {
    
    private ShenyuRequestHeader header;

    private ShenyuRequestParameter parameter;

    private ShenyuCookie cookie;

    /**
     * get header.
     *
     * @return header
     */
    public ShenyuRequestHeader getHeader() {
        return header;
    }

    /**
     * set header.
     *
     * @param header header
     */
    public void setHeader(final ShenyuRequestHeader header) {
        this.header = header;
    }

    /**
     * get parameter.
     *
     * @return parameter
     */
    public ShenyuRequestParameter getParameter() {
        return parameter;
    }

    /**
     * set parameter.
     *
     * @param parameter parameter
     */
    public void setParameter(final ShenyuRequestParameter parameter) {
        this.parameter = parameter;
    }

    /**
     * get cookie.
     *
     * @return cookie
     */
    public ShenyuCookie getCookie() {
        return cookie;
    }

    /**
     * set cookie.
     *
     * @param cookie cookie
     */
    public void setCookie(final ShenyuCookie cookie) {
        this.cookie = cookie;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        RequestHandle that = (RequestHandle) o;
        return Objects.equals(header, that.header) && Objects.equals(parameter, that.parameter) && Objects.equals(cookie, that.cookie);
    }

    @Override
    public int hashCode() {
        return Objects.hash(header, parameter, cookie);
    }

    @Override
    public String toString() {
        return "RequestHandle{"
                + "header="
                + header
                + ", parameter="
                + parameter
                + ", cookie="
                + cookie
                + '}';
    }

    /**
     * is empty config.
     *
     * @return empty is true
     */
    public boolean isEmptyConfig() {
        return !isNotEmptyConfig();
    }

    /**
     * is not empty config.
     *
     * @return not empty is true
     */
    private boolean isNotEmptyConfig() {
        return header.isNotEmptyConfig() || parameter.isNotEmptyConfig() || cookie.isNotEmptyConfig();
    }

    public class ShenyuRequestHeader {
        /**
         * need to be appended new header value.
         */
        private Map<String, String> addHeaders;

        /**
         * new headerKey replaces old headerKey.
         * key: oldHeaderKey, value: newHeaderKey.
         */
        private Map<String, String> replaceHeaderKeys;

        /**
         * need to be covered header value.
         * key: oldHeaderKey, value: newHeaderValue.
         */
        private Map<String, String> setHeaders;

        /**
         * need to be removed headerKey.
         */
        private Set<String> removeHeaderKeys;

        /**
         * no args constructor.
         */
        public ShenyuRequestHeader() {
        }

        /**
         * all args constructor.
         *
         * @param addHeaders        addHeaders
         * @param replaceHeaderKeys replaceHeaderKeys
         * @param setHeaders        setHeaders
         * @param removeHeaderKeys  removeHeaderKeys
         */
        public ShenyuRequestHeader(final Map<String, String> addHeaders, final Map<String, String> replaceHeaderKeys,
                                   final Map<String, String> setHeaders, final Set<String> removeHeaderKeys) {
            this.addHeaders = addHeaders;
            this.replaceHeaderKeys = replaceHeaderKeys;
            this.setHeaders = setHeaders;
            this.removeHeaderKeys = removeHeaderKeys;
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

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            ShenyuRequestHeader that = (ShenyuRequestHeader) o;
            return Objects.equals(addHeaders, that.addHeaders) && Objects.equals(replaceHeaderKeys, that.replaceHeaderKeys)
                    && Objects.equals(setHeaders, that.setHeaders) && Objects.equals(removeHeaderKeys, that.removeHeaderKeys);
        }

        @Override
        public int hashCode() {
            return Objects.hash(addHeaders, replaceHeaderKeys, setHeaders, removeHeaderKeys);
        }

        @Override
        public String toString() {
            return "ShenyuRequestHeader{"
                    + "addHeaders="
                    + addHeaders
                    + ", replaceHeaderKeys="
                    + replaceHeaderKeys
                    + ", setHeaders="
                    + setHeaders
                    + ", removeHeaderKeys="
                    + removeHeaderKeys
                    + '}';
        }

        /**
         * is not empty config.
         *
         * @return not empty is true
         */
        public boolean isNotEmptyConfig() {
            return MapUtils.isNotEmpty(addHeaders) || MapUtils.isNotEmpty(replaceHeaderKeys)
                    || MapUtils.isNotEmpty(setHeaders) || CollectionUtils.isNotEmpty(removeHeaderKeys);
        }
    }

    public class ShenyuRequestParameter {

        private Map<String, String> addParameters;

        private Map<String, String> replaceParameterKeys;

        private Map<String, String> setParameters;

        private Set<String> removeParameterKeys;

        /**
         * no args constructor.
         */
        public ShenyuRequestParameter() {
        }

        /**
         * all args constructor.
         *
         * @param addParameters        addParameters
         * @param replaceParameterKeys replaceParameterKeys
         * @param setParameters        setParameters
         * @param removeParameterKeys  removeParameterKeys
         */
        public ShenyuRequestParameter(final Map<String, String> addParameters, final Map<String, String> replaceParameterKeys,
                                      final Map<String, String> setParameters, final Set<String> removeParameterKeys) {
            this.addParameters = addParameters;
            this.replaceParameterKeys = replaceParameterKeys;
            this.setParameters = setParameters;
            this.removeParameterKeys = removeParameterKeys;
        }

        /**
         * get addParameters.
         *
         * @return addParameters
         */
        public Map<String, String> getAddParameters() {
            return addParameters;
        }

        /**
         * set addParameters.
         *
         * @param addParameters addParameters
         */
        public void setAddParameters(final Map<String, String> addParameters) {
            this.addParameters = addParameters;
        }

        /**
         * get replaceParameterKeys.
         *
         * @return replaceParameterKeys
         */
        public Map<String, String> getReplaceParameterKeys() {
            return replaceParameterKeys;
        }

        /**
         * set replaceParameterKeys.
         *
         * @param replaceParameterKeys replaceParameterKeys
         */
        public void setReplaceParameterKeys(final Map<String, String> replaceParameterKeys) {
            this.replaceParameterKeys = replaceParameterKeys;
        }

        /**
         * get setParameters.
         *
         * @return setParameters
         */
        public Map<String, String> getSetParameters() {
            return setParameters;
        }

        /**
         * set setParameters.
         *
         * @param setParameters setParameters
         */
        public void setSetParameters(final Map<String, String> setParameters) {
            this.setParameters = setParameters;
        }

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

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            ShenyuRequestParameter that = (ShenyuRequestParameter) o;
            return Objects.equals(addParameters, that.addParameters) && Objects.equals(replaceParameterKeys, that.replaceParameterKeys)
                    && Objects.equals(setParameters, that.setParameters) && Objects.equals(removeParameterKeys, that.removeParameterKeys);
        }

        @Override
        public String toString() {
            return "ShenyuRequestParameter{"
                    + "addParameters="
                    + addParameters
                    + ", replaceParameterKeys="
                    + replaceParameterKeys
                    + ", setParameters="
                    + setParameters
                    + ", removeParameterKeys="
                    + removeParameterKeys
                    + '}';
        }

        @Override
        public int hashCode() {
            return Objects.hash(addParameters, replaceParameterKeys, setParameters, removeParameterKeys);
        }

        /**
         * is not empty config.
         *
         * @return not empty is true
         */
        public boolean isNotEmptyConfig() {
            return MapUtils.isNotEmpty(addParameters) || MapUtils.isNotEmpty(replaceParameterKeys)
                    || MapUtils.isNotEmpty(setParameters) || CollectionUtils.isNotEmpty(removeParameterKeys);
        }
    }

    public class ShenyuCookie {
        private Map<String, String> addCookies;

        private Map<String, String> replaceCookieKeys;

        private Map<String, String> setCookies;

        private Set<String> removeCookieKeys;

        /**
         * no args constructor.
         */
        public ShenyuCookie() {
        }

        /**
         * all args constructor.
         *
         * @param addCookies        addCookies
         * @param replaceCookieKeys replaceCookieKeys
         * @param setCookies        setCookies
         * @param removeCookieKeys  removeCookieKeys
         */
        public ShenyuCookie(final Map<String, String> addCookies, final Map<String, String> replaceCookieKeys,
                            final Map<String, String> setCookies, final Set<String> removeCookieKeys) {
            this.addCookies = addCookies;
            this.replaceCookieKeys = replaceCookieKeys;
            this.setCookies = setCookies;
            this.removeCookieKeys = removeCookieKeys;
        }

        /**
         * get addCookies.
         *
         * @return addCookies
         */
        public Map<String, String> getAddCookies() {
            return addCookies;
        }

        /**
         * set addCookies.
         *
         * @param addCookies addCookies
         */
        public void setAddCookies(final Map<String, String> addCookies) {
            this.addCookies = addCookies;
        }

        /**
         * get replaceCookieKeys.
         *
         * @return replaceCookieKeys
         */
        public Map<String, String> getReplaceCookieKeys() {
            return replaceCookieKeys;
        }

        /**
         * set replaceCookieKeys.
         *
         * @param replaceCookieKeys replaceCookieKeys
         */
        public void setReplaceCookieKeys(final Map<String, String> replaceCookieKeys) {
            this.replaceCookieKeys = replaceCookieKeys;
        }

        /**
         * get setCookies.
         *
         * @return setCookies
         */
        public Map<String, String> getSetCookies() {
            return setCookies;
        }

        /**
         * set setCookies.
         *
         * @param setCookies setCookies
         */
        public void setSetCookies(final Map<String, String> setCookies) {
            this.setCookies = setCookies;
        }

        /**
         * get removeCookieKeys.
         *
         * @return removeCookieKeys
         */
        public Set<String> getRemoveCookieKeys() {
            return removeCookieKeys;
        }

        /**
         * set removeCookieKeys.
         *
         * @param removeCookieKeys removeCookieKeys
         */
        public void setRemoveCookieKeys(final Set<String> removeCookieKeys) {
            this.removeCookieKeys = removeCookieKeys;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            ShenyuCookie that = (ShenyuCookie) o;
            return Objects.equals(addCookies, that.addCookies) && Objects.equals(replaceCookieKeys, that.replaceCookieKeys)
                    && Objects.equals(setCookies, that.setCookies) && Objects.equals(removeCookieKeys, that.removeCookieKeys);
        }

        @Override
        public int hashCode() {
            return Objects.hash(addCookies, replaceCookieKeys, setCookies, removeCookieKeys);
        }

        @Override
        public String toString() {
            return "ShenyuCookie{"
                    + "addCookies="
                    + addCookies
                    + ", replaceCookieKeys="
                    + replaceCookieKeys
                    + ", setCookies="
                    + setCookies
                    + ", removeCookieKeys="
                    + removeCookieKeys
                    + '}';
        }

        /**
         * is not empty config.
         *
         * @return not empty is true
         */
        public boolean isNotEmptyConfig() {
            return MapUtils.isNotEmpty(addCookies) || MapUtils.isNotEmpty(replaceCookieKeys)
                    || MapUtils.isNotEmpty(setCookies) || CollectionUtils.isNotEmpty(removeCookieKeys);
        }
    }
}
