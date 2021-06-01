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

package org.apache.shenyu.common.dto.convert;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.common.utils.CollectionUtils;

import java.util.Map;
import java.util.Set;

/**
 * this is RequestHandle plugin handle.
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class RequestHandle {
    private ShenyuRequestHeader header;

    private ShenyuRequestParameter parameter;

    private ShenyuCookie cookie;

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

    @Getter
    @Setter
    @ToString
    @NoArgsConstructor
    @AllArgsConstructor
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
         * is not empty config.
         *
         * @return not empty is true
         */
        public boolean isNotEmptyConfig() {
            return MapUtils.isNotEmpty(addHeaders) || MapUtils.isNotEmpty(replaceHeaderKeys)
                    || MapUtils.isNotEmpty(setHeaders) || CollectionUtils.isNotEmpty(removeHeaderKeys);
        }
    }

    @Getter
    @Setter
    @ToString
    @NoArgsConstructor
    @AllArgsConstructor
    public class ShenyuRequestParameter {
        private Map<String, String> addParameters;

        private Map<String, String> replaceParameterKeys;

        private Map<String, String> setParameters;

        private Set<String> removeParameterKeys;

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

    @Getter
    @Setter
    @ToString
    @NoArgsConstructor
    @AllArgsConstructor
    public class ShenyuCookie {
        private Map<String, String> addCookies;

        private Map<String, String> replaceCookieKeys;

        private Map<String, String> setCookies;

        private Set<String> removeCookieKeys;

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
