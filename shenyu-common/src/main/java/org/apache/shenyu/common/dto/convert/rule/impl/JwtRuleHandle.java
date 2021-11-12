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

/**
 * Jwt rule handle.
 */
public class JwtRuleHandle implements RuleHandle {

    /**
     * converter, Jwt's body content is assigned to the header.
     */
    private List<Convert> converter;

    /**
     * get converter.
     * @return List
     */
    public List<Convert> getConverter() {
        return converter;
    }

    @Override
    public String toString() {
        return "JwtRuleHandle{"
                + "converter=" + converter.toString()
                + '}';
    }

    /**
     * set converter.
     * @param converter converter
     */
    public void setConverter(final List<Convert> converter) {
        this.converter = converter;
    }

    public static class Convert {

        /**
         * jwt of body name.
         */
        private String jwtVal;

        /**
         * header name.
         */
        private String headerVal;

        /**
         * get jwtVal.
         * @return jwtVal
         */
        public String getJwtVal() {
            return jwtVal;
        }

        /**
         * set jwtVal.
         * @param jwtVal jwtVal
         */
        public void setJwtVal(final String jwtVal) {
            this.jwtVal = jwtVal;
        }

        /**
         * get headerVal.
         * @return headerVal
         */
        public String getHeaderVal() {
            return headerVal;
        }

        /**
         * set headerVal.
         * @param headerVal headerVal
         */
        public void setHeaderVal(final String headerVal) {
            this.headerVal = headerVal;
        }

        @Override
        public String toString() {
            return "Convert{"
                    + "jwtVal='" + jwtVal + '\''
                    + ", headerVal='" + headerVal + '\''
                    + '}';
        }
    }
}
